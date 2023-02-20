package uk.co.nstauthority.scap.fds.searchselector;

import java.util.List;

public record ManualEntryPartitionedList(List<Integer> ids,
                                         List<String> manualEntries) {
}
