#[derive(Debug, PartialEq)]
struct Packet {
    version: u64,
    type_id: u64,
    payload: Payload,
}

impl Packet {
    fn get_versions(&self) -> Vec<u64> {
        let mut versions = vec![self.version];

        match &self.payload {
            Payload::Literal(_) => {}
            Payload::Operator(packets) => {
                for packet in packets {
                    versions.extend(packet.get_versions());
                }
            }
        }

        versions
    }

    fn calculate_expression(&self) -> u64 {
        match &self.payload {
            Payload::Literal(value) => *value,
            Payload::Operator(packets) => match self.type_id {
                0 => packets.iter().map(Self::calculate_expression).sum::<u64>(),
                1 => packets
                    .iter()
                    .map(Self::calculate_expression)
                    .product::<u64>(),
                2 => packets
                    .iter()
                    .map(Self::calculate_expression)
                    .min()
                    .unwrap(),
                3 => packets
                    .iter()
                    .map(Self::calculate_expression)
                    .max()
                    .unwrap(),
                5 => (packets[0].calculate_expression() > packets[1].calculate_expression()) as u64,
                6 => (packets[0].calculate_expression() < packets[1].calculate_expression()) as u64,
                7 => {
                    (packets[0].calculate_expression() == packets[1].calculate_expression()) as u64
                }
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug, PartialEq)]
enum Payload {
    Literal(u64),
    Operator(Vec<Packet>),
}

#[derive(Debug, PartialEq)]
enum OperatorType {
    Zero(usize),
    One(usize),
}

fn take_n(input: &str, n: usize) -> &str {
    &input[..n]
}

fn drop_n(input: &str, n: usize) -> &str {
    &input[n..]
}

fn to_int(value: &str) -> u64 {
    u64::from_str_radix(value, 2).unwrap()
}

fn parse_version(input: &str) -> (Option<u64>, &str) {
    if input.len() < 3 {
        return (None, input);
    }
    (Some(to_int(take_n(input, 3))), drop_n(input, 3))
}

fn parse_type_id(input: &str) -> (Option<u64>, &str) {
    if input.len() < 3 {
        return (None, input);
    }
    (Some(to_int(take_n(input, 3))), drop_n(input, 3))
}

fn parse_packet(input: &str) -> (Option<Packet>, &str) {
    let starting_input = input;

    let (version, input) = parse_version(input);
    if version.is_none() {
        return (None, input);
    };
    let version = version.unwrap();

    let (type_id, input) = parse_type_id(input);
    if type_id.is_none() {
        return (None, input);
    }
    let type_id = type_id.unwrap();

    if type_id != 4 {
        // operator packet
        let mut packets = vec![];
        let mut input = input;

        match parse_operator(input) {
            (OperatorType::Zero(subpackets_size), next_input) => {
                let mut subpackages_input = take_n(next_input, subpackets_size);
                while let (Some(packet), subpackages_leftover) = parse_packet(subpackages_input) {
                    subpackages_input = subpackages_leftover;
                    packets.push(packet);
                }

                input = drop_n(next_input, subpackets_size - subpackages_input.len());
            }
            (OperatorType::One(mut package_count), next_input) => {
                input = next_input;

                while let (Some(packet), leftover) = parse_packet(input) {
                    if package_count == 0 {
                        break;
                    }
                    package_count -= 1;
                    packets.push(packet);
                    input = leftover;
                }

                if package_count > 0 {
                    return (None, starting_input);
                }
            }
        }

        debug_assert!(!packets.is_empty());

        let packet = Packet {
            version,
            type_id,
            payload: Payload::Operator(packets),
        };

        (Some(packet), input)
    } else {
        // literal packet
        let (value, input) = parse_literal_value(input);
        if value.is_none() {
            return (None, input);
        }
        let value = value.unwrap();

        let packet = Packet {
            version,
            type_id,
            payload: Payload::Literal(value),
        };
        (Some(packet), input)
    }
}

fn parse_operator(mut input: &str) -> (OperatorType, &str) {
    //An operator packet contains one or more packets

    let type_length = take_n(input, 1);
    input = drop_n(input, 1);

    if type_length == "0" {
        //the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
        let subpackets_size = u64::from_str_radix(take_n(input, 15), 2).unwrap() as usize;
        input = drop_n(input, 15);

        (OperatorType::Zero(subpackets_size), input)
    } else {
        // the next 11 bits give the number of following packets
        let packets_count = u64::from_str_radix(take_n(input, 11), 2).unwrap() as usize;
        input = drop_n(input, 11);

        (OperatorType::One(packets_count), input)
    }
}

fn parse_literal_value(mut input: &str) -> (Option<u64>, &str) {
    let mut value: Vec<char> = vec![];

    loop {
        let chunk = take_n(input, 5);
        input = drop_n(input, 5);

        if chunk.is_empty() {
            break;
        }

        value.extend(drop_n(chunk, 1).chars());

        if take_n(chunk, 1) == "0" {
            break;
        }
    }

    let value = value.iter().collect::<String>();
    let value = u64::from_str_radix(&value, 2).ok();

    (value, input)
}

fn hex_to_binary(hex_digit: char) -> &'static str {
    match hex_digit {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'A' => "1010",
        'B' => "1011",
        'C' => "1100",
        'D' => "1101",
        'E' => "1110",
        'F' => "1111",
        _ => unreachable!(),
    }
}

fn main() {
    let message: String = include_str!("input")
        .chars()
        .flat_map(|h| hex_to_binary(h).chars())
        .collect();

    let (packet, _leftover) = parse_packet(message.as_str());
    let packet = packet.unwrap();

    println!("Part 1: {}", packet.get_versions().iter().sum::<u64>());

    println!("Part 2: {}", packet.calculate_expression());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_literal_value() {
        assert_eq!(
            (Some(2021), "000"),
            parse_literal_value("101111111000101000")
        );
        assert_eq!((Some(2021), ""), parse_literal_value("101111111000101"));
    }

    #[test]
    fn test_parse_operator() {
        assert_eq!(
            (OperatorType::Zero(27), "1101000101001010010001001000000000"),
            parse_operator("00000000000110111101000101001010010001001000000000")
        );
        assert_eq!(
            (
                OperatorType::One(3),
                "01010000001100100000100011000001100000"
            ),
            parse_operator("10000000001101010000001100100000100011000001100000")
        );
    }

    #[test]
    fn test_parse_operator_type0_packet_with_two_literal_subpackages() {
        let packet_bits = "00111000000000000110111101000101001010010001001000000000";
        let (packet, leftover) = parse_packet(packet_bits);
        let packet = packet.unwrap();

        assert_eq!(1, packet.version);
        assert_eq!(6, packet.type_id);
        assert!(matches!(packet.payload, Payload::Operator(_)));
        assert_eq!("0000000", leftover);

        if let Payload::Operator(packets) = packet.payload {
            assert_eq!(2, packets.len());
            assert_eq!(
                Packet {
                    version: 6,
                    type_id: 4,
                    payload: Payload::Literal(10),
                },
                packets[0]
            );
            assert_eq!(
                Packet {
                    version: 2,
                    type_id: 4,
                    payload: Payload::Literal(20),
                },
                packets[1]
            );
        }
    }

    #[test]
    fn test_parse_operator_type1_packet_with_three_literal_subpackages() {
        let packet_bits = "11101110000000001101010000001100100000100011000001100000";
        let (packet, leftover) = parse_packet(packet_bits);
        let packet = packet.unwrap();

        assert_eq!(7, packet.version);
        assert_eq!(3, packet.type_id);
        assert!(matches!(packet.payload, Payload::Operator(_)));
        assert_eq!("00000", leftover);

        if let Payload::Operator(packets) = packet.payload {
            assert_eq!(3, packets.len());
            assert_eq!(
                Packet {
                    version: 2,
                    type_id: 4,
                    payload: Payload::Literal(1),
                },
                packets[0]
            );
            assert_eq!(
                Packet {
                    version: 4,
                    type_id: 4,
                    payload: Payload::Literal(2),
                },
                packets[1]
            );
            assert_eq!(
                Packet {
                    version: 1,
                    type_id: 4,
                    payload: Payload::Literal(3),
                },
                packets[2]
            );
        }
    }
}
