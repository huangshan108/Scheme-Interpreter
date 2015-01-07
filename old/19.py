from ucb import trace

#@trace
lst = []
def count_partitions(total, max_pieces, max_value):
    """Count the ways to partition n using parts up to m."""
    #print(str(total) + ' ' + str(max_value))
    if total == 0:
        return 1
    elif total < 0:
        return 0
    elif max_value == 0:
        return 0
    else:
        if total - max_value >= 0:
            lst.append(max_value)
        return count_partitions(total - max_value, max_pieces, max_value) + count_partitions(total, max_pieces, max_value - 1)

