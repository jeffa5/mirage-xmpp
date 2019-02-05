import argparse
import math
import re
import xml.etree.ElementTree as ET
from collections import defaultdict
from enum import Enum
from pprint import pprint

import matplotlib.pyplot as plt
import numpy as np


def parse_newclient(line):
    """NewClient:<time>:<id>

    :param line:

    """
    return float(line.split(":")[1])


def parse_endclient(line):
    """EndClient:<time>:<id?>

    :param line:

    """
    return float(line.split(":")[1])


def parse_load(line):
    """load:<num>

    :param line:

    """
    return int(line.split(":")[-1])


class Xml(Enum):
    """Enum for the xml data."""

    StreamHeader = 1
    StreamClose = 2
    Auth = 3
    RosterGet = 4
    RosterPush = 5
    RosterSet = 6
    ResourceBind = 7
    Session = 8
    PresenceUpdate = 9
    StreamFeatures = 10
    SaslSuccess = 11
    BindResult = 12
    IqResult = 13
    RosterResult = 14
    Registration = 15
    SaslFailure = 16
    PresenceSubscribe = 17
    PresenceSubscribed = 18
    PresenceUnavailable = 19


def read_to_end(xml_list, name):
    """

    :param xml_list:
    :param name:
    :returns: next actual stanza.

    """
    start_index = 0
    end_index = 0
    for i, item in enumerate(xml_list):
        if item[0] == "start":
            if name == item[1].tag:
                start_index = i
        elif item[0] == "end":
            if name == item[1].tag:
                end_index = i
                break
    return end_index - start_index


def no_ns(name):
    """

    :param name:

    """
    return re.sub("{.*?}", "", name)


def parse_xml(xml, parser, is_recv):
    """Parse xml data into an enum.

    :param xml:
    :param parser:

    """
    parser.feed(xml)
    events = list(parser.read_events())
    return_events = []
    skip = 0
    for event, elem in events:
        if skip > 0:
            skip -= 1
            continue
        skip = read_to_end(events, elem.tag)
        name = no_ns(elem.tag)
        if "stream" in name:
            skip = 0
            if event == "start":
                return_events.append(Xml.StreamHeader)
            elif event == "end":
                return_events.append(Xml.StreamClose)
        elif "features" in name:
            return_events.append(Xml.StreamFeatures)
        elif "auth" in name:
            return_events.append(Xml.Auth)
        elif "success" in name:
            return_events.append(Xml.SaslSuccess)
        elif "failure" in name:
            return_events.append(Xml.SaslFailure)
        elif "iq" in name:
            if "set" == elem.get("type"):
                children = list(elem)
                if "bind" in children[0].tag:
                    return_events.append(Xml.ResourceBind)
                elif "session" in children[0].tag:
                    return_events.append(Xml.Session)
                elif "register" in children[0].tag:
                    return_events.append(Xml.Registration)
                elif "query" in children[0].tag:
                    if is_recv:
                        return_events.append(Xml.RosterPush)
                    else:
                        return_events.append(Xml.RosterSet)
            elif "get" == elem.get("type"):
                children = list(elem)
                if "query" in children[0].tag:
                    if "roster" in children[0].tag:
                        return_events.append(Xml.RosterGet)
            elif "result" == elem.get("type"):
                children = list(elem)
                if children:
                    if "bind" in children[0].tag:
                        return_events.append(Xml.BindResult)
                    elif "query" in children[0].tag:
                        return_events.append(Xml.RosterResult)
                else:
                    return_events.append(Xml.IqResult)
        if "presence" in name:
            if "unavailable" == elem.get("type"):
                return_events.append(Xml.PresenceUnavailable)
            elif "subscribe" == elem.get("type"):
                return_events.append(Xml.PresenceSubscribe)
            elif "subscribed" == elem.get("type"):
                return_events.append(Xml.PresenceSubscribed)
            elif None is elem.get("type"):
                return_events.append(Xml.PresenceUpdate)
    return return_events


def parse_send(line, parsers):
    """Send:<time>:<id>:<xml>

    :param line:
    :param parsers:

    """
    split = line.split(":")
    time = float(split[1])
    identifier = split[2][1:-1]
    if identifier not in parsers or "<?xml" in ":".join(split[3:]):
        parser = ET.XMLPullParser(["start", "end"])
        parsers[identifier] = parser
    xml = parse_xml(":".join(split[3:]), parsers[identifier], False)
    return identifier, [(time, x) for x in xml]


def parse_recv(lines, parsers):
    """Recv:<time>:<id>:<xml>

    :param lines:
    :param parsers:

    """
    split = lines[0].split(":")
    time = float(split[1])
    identifier = split[2][1:-1]
    if identifier not in parsers or "<?xml" in ":".join(split[3:]):
        parser = ET.XMLPullParser(["start", "end"])
        parsers[identifier] = parser
    xml = parse_xml(":".join(split[3:]) + "".join(lines[1:]), parsers[identifier], True)
    return identifier, [(time, x) for x in xml]


def get_args():
    """Parse the program arguments to get the file to read from."""
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="+")
    # save figs
    parser.add_argument("-s", "--save", action="store_const", const=True, default=False)
    return parser.parse_args()


def plot_load(loads):
    """

    :param loads:

    """
    plt.figure()
    plt.plot(loads.keys(), loads.values())
    plt.xlabel("Time (s)")
    plt.ylabel("Load")
    plt.title("User load over time")


def plot_duration(send_data):
    """

    :param send_data:

    """
    plt.figure()
    plt.plot(send_data.keys(), [sd[-1][0] - sd[0][0] for sd in send_data.values()])
    plt.xlabel("id")
    plt.ylabel("duration")
    plt.title("Duration of send data connections")


def plot_duration_scatter(send_data, recv_data):
    """

    :param send_data:
    :param recv_data:

    """
    plt.figure()
    sd, rd = [], []
    for key in send_data:
        if send_data[key][-1][0] != send_data[key][0][0]:
            sd.append(send_data[key][-1][0] - send_data[key][0][0])
            rd.append(recv_data[key][-1][0] - recv_data[key][0][0])
    plt.scatter(sd, rd, s=4)
    plt.xlabel("Send duration")
    plt.ylabel("Recv duration")
    plt.title("Duration of connections")


def get_latencies(send_data, recv_data):
    latencies_dict = defaultdict(list)
    base_time = min([time_item_list[0][0] for time_item_list in send_data.values()])
    unmatched = defaultdict(int)
    for identifier in send_data:
        for send_time, send_item in send_data[identifier]:
            if send_item == Xml.PresenceSubscribed:
                continue
            elif send_item == Xml.PresenceUpdate:
                continue
            elif send_item == Xml.PresenceUnavailable:
                continue
            matched = False
            for recv_time, recv_item in recv_data[identifier]:
                if send_time > recv_time:
                    continue
                if send_item == Xml.StreamHeader:
                    if recv_item == Xml.StreamHeader:
                        matched = True
                elif send_item == Xml.StreamClose:
                    if recv_item == Xml.StreamClose:
                        matched = True
                elif send_item == Xml.PresenceSubscribe:
                    if recv_item == Xml.PresenceSubscribed:
                        matched = True
                elif send_item == Xml.RosterSet:
                    if recv_item == Xml.RosterPush:
                        matched = True
                elif send_item == Xml.RosterGet:
                    if recv_item == Xml.RosterResult:
                        matched = True
                elif send_item == Xml.Auth:
                    if recv_item == Xml.SaslSuccess:
                        matched = True
                    if recv_item == Xml.SaslFailure:
                        matched = True
                elif send_item == Xml.ResourceBind:
                    if recv_item == Xml.BindResult:
                        matched = True
                elif send_item == Xml.Session:
                    if recv_item == Xml.IqResult:
                        matched = True

                if matched:
                    latencies_dict[(send_item, recv_item)].append(
                        (send_time - base_time, recv_time - send_time)
                    )
                    break
                else:
                    unmatched[(send_item.name, recv_item.name)] += 1
    pprint(dict(unmatched))
    return latencies_dict


def plot_latency(latencies_dict):
    plt.figure()

    for (key, item) in latencies_dict.items():
        times = [x[0] for x in item]
        latencies = [x[1] for x in item]
        plt.scatter(
            times, latencies, s=8, alpha=0.8, label=f"{key[0].name} -> {key[1].name}"
        )
    plt.xlabel("Times of sent messages (s)")
    plt.ylabel("Latency (s)")
    plt.ylim(bottom=0)
    plt.legend()
    plt.title("Latency of messages to server")


def plot_latency_mavg(latencies_dict):
    plt.figure()
    latencies = []
    for value in latencies_dict.values():
        latencies.extend(value)

    latencies = sorted(latencies, key=lambda x: x[0])

    n = 100

    def cumsum_pair(it):
        total = 0
        for x in it:
            total += x[1]
            yield (x[0], total)

    cumulative_latencies = list(cumsum_pair(latencies))
    average_latencies = []
    for i in range(n, len(cumulative_latencies) - n):
        average_latencies.append(
            (
                cumulative_latencies[i][0],
                (cumulative_latencies[i + n][1] - cumulative_latencies[i - n][1]) / n,
            )
        )

    times = [x[0] for x in average_latencies]
    latencies = [x[1] for x in average_latencies]
    plt.plot(times, latencies)
    plt.xlabel("Times of sent messages (s)")
    plt.ylabel("average Latency (s)")
    plt.title("average Latency of messages to server")


def plot_latency_percentiles(latencies_dict):
    plt.figure()
    latencies = defaultdict(list)
    for key in latencies_dict:
        for time, latency in latencies_dict[key]:
            latencies[int(time)].append(latency)

    def plot_percentile_latency(latencies, percentile):
        """

        :param latencies:
        :param percentile:

        """
        percentile_latencies = sorted(
            [(t, np.percentile(l, percentile)) for t, l in latencies.items()]
        )
        plt.plot(
            [x[0] for x in percentile_latencies],
            [x[1] for x in percentile_latencies],
            label=str(percentile),
        )

    plot_percentile_latency(latencies, 100)
    plot_percentile_latency(latencies, 90)
    plot_percentile_latency(latencies, 80)
    plot_percentile_latency(latencies, 70)
    plot_percentile_latency(latencies, 60)
    plot_percentile_latency(latencies, 50)

    plt.legend()

    plt.xlabel("Times of sent messages")
    plt.ylabel("Latency (s)")
    plt.title("Latency of messages to server")


def plot_throughput(send_data, recv_data):
    """

    :param send_data:
    :param recv_data:

    """
    plt.figure()
    base_time = list(send_data.values())[0][0][0]
    times = [item[0] - base_time for sd in send_data.values() for item in sd]
    hist = np.histogram(times, bins=list(range(0, 1 + int(math.ceil(times[-1])))))
    throughput = [i / j for i, j in zip(hist[0], np.diff(hist[1]))]
    bar_centers = (hist[1][1:] + hist[1][:-1]) / 2
    plt.plot(bar_centers, throughput, label="Send")

    base_time = list(recv_data.values())[0][0][0]
    times = [item[0] - base_time for sd in recv_data.values() for item in sd]
    hist = np.histogram(times, bins=list(range(0, 1 + int(math.ceil(times[-1])))))
    throughput = [i / j for i, j in zip(hist[0], np.diff(hist[1]))]
    bar_centers = (hist[1][1:] + hist[1][:-1]) / 2
    plt.plot(bar_centers, throughput, label="Recv")

    plt.xlabel("Time (s)")
    plt.ylabel("Throughput (msg/s)")
    plt.title("Throughput of the server")
    plt.legend()


def plot_cpu(cpu_data):
    plt.figure()
    plt.plot(list(cpu_data.keys()), list(cpu_data.values()))

    plt.title("CPU load during test")
    plt.xlabel("Time (s)")
    plt.ylabel("%CPU")


def plot_mem(mem_data):
    plt.figure()
    plt.plot(list(mem_data.keys()), list(mem_data.values()))

    plt.title("Mem load during test")
    plt.xlabel("Time (s)")
    plt.ylabel("%Mem")


def main():
    """ """
    args = get_args()

    for file in args.files:
        print(file)
        recv_parsers = {}
        send_parsers = {}
        with open(file + "/dump", "r") as f:
            lines = f.readlines()
            # time -> load
            new_clients = {}
            # time -> load
            end_clients = {}
            loads = {}
            # id -> [(time, xml)]
            send_data = defaultdict(list)
            # id -> [(time, xml)]
            recv_data = defaultdict(list)

            line_index = 0
            while line_index < len(lines):
                line = lines[line_index].strip()
                line_index += 1
                if line:
                    if line.startswith("NewClient"):
                        time = parse_newclient(line)
                        load = parse_load(lines[line_index].strip())
                        line_index += 1
                        new_clients[time] = load
                        loads[time] = load
                    elif line.startswith("EndClient"):
                        time = parse_endclient(line)
                        if line_index < len(lines):
                            load = parse_load(lines[line_index].strip())
                            line_index += 1
                            end_clients[time] = load
                            loads[time] = load
                    elif line.startswith("Send"):
                        key, val = parse_send(line, send_parsers)
                        send_data[key].extend(val)
                    elif line.startswith("Recv"):
                        xml_lines = [line]
                        if line_index < len(lines):
                            l = lines[line_index].strip()
                            while l.startswith("<"):
                                line_index += 1
                                xml_lines.append(l)
                                l = lines[line_index].strip()
                        key, val = parse_recv(xml_lines, recv_parsers)
                        recv_data[key].extend(val)
                    elif line.startswith("Element"):
                        pass
                    else:
                        print("UNMATCHED:", line)

        base_time = list(loads)[0]
        loads = {t - base_time: l for t, l in loads.items()}

        def save_or_show(filename):
            if args.save:
                plt.savefig(file + filename, dpi=900)
            else:
                plt.show()
            plt.close()

        # load over time
        plot_load(loads)
        save_or_show("/load.png")

        # duration of transactions
        # plot_duration(send_data)
        # if not args.q:
        #     plot.show()

        # scatter plot of durations
        # plot_duration_scatter(send_data, recv_data)
        # if not args.q:
        #     plt.show()
        # plt.savefig(file + "/duration.png")

        latencies_dict = get_latencies(send_data, recv_data)
        # latency = time between the send and receive
        plot_latency(latencies_dict)
        save_or_show("/latency.png")

        plot_latency_mavg(latencies_dict)
        save_or_show("/latency_mavg.png")

        plot_latency_percentiles(latencies_dict)
        save_or_show("/latency_percentiles.png")

        # throughput is the number of messages per unit time
        plot_throughput(send_data, recv_data)
        save_or_show("/throughput.png")

        with open(file + "/cpumem", "r") as f:
            lines = f.readlines()
            cpu_data = {}
            mem_data = {}
            base_time = None

            for line in lines:
                parts = line.split()
                if base_time is None:
                    base_time = float(parts[0])
                time = float(parts[0]) - base_time
                cpu_data[time] = float(parts[1])
                mem_data[time] = float(parts[2])

        plot_cpu(cpu_data)
        save_or_show("/cpu.png")

        plot_mem(mem_data)
        save_or_show("/mem.png")


main()
