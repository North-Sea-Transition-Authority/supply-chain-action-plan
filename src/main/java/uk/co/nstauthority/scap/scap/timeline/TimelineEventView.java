package uk.co.nstauthority.scap.scap.timeline;

public record TimelineEventView(String timelineEventSubject,
                                Integer scapId,
                                Integer versionNumber,
                                String formattedTime,
                                String userDisplayName) {
}
