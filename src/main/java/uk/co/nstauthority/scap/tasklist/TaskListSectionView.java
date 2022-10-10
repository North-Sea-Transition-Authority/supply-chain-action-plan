package uk.co.nstauthority.scap.tasklist;

import java.util.List;

/**
 * This record is used to display sections on the frontend in the task list based on {@link TaskListSection}.
 */
public record TaskListSectionView(
    int displayOrder,
    String sectionName,
    String sectionWarningText,
    List<TaskListItemView> taskListItemViews
) {}
