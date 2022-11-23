package uk.co.nstauthority.scap.util;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListItemView;
import uk.co.nstauthority.scap.tasklist.TaskListSection;
import uk.co.nstauthority.scap.tasklist.TaskListSectionView;

public class TaskListSectionUtil {

  private TaskListSectionUtil() {
    throw new IllegalUtilClassInstantiationException(TaskListSectionUtil.class);
  }

  /**
   * Create a section view for a given section to be displayed on the frontend. Used from a section level.
   *
   * @param section The section to generate the {@link TaskListSectionView} for
   * @param taskListItems A list of {@link TaskListItem}
   * @param target The generic type used for the section and item implementations
   * @param <T> Helps to ensure type safety is kept when passing in the given object.
   * @return A generated {@link TaskListSectionView} object
   */
  public static <T> TaskListSectionView createSectionView(TaskListSection<T> section,
                                                          Collection<? extends TaskListItem<T>> taskListItems,
                                                          T target) {
    var itemViews = taskListItems
        .stream()
        .filter(taskListItem -> taskListItem.getTaskListSection().equals(section.getClass()))
        .filter(taskListItem -> taskListItem.isVisible(target))
        .map(item -> createTaskListItemView(item, target))
        .sorted(Comparator.comparing(TaskListItemView::getDisplayOrder))
        .toList();

    return createTaskListSectionView(section, itemViews);
  }

  /**
   * Create section views when given a list of sections. Used from a controller level.
   *
   * @param taskListSections The sections to generate a view for.
   * @param taskListItems The items to include on the task list.
   * @param target The object to pass into the task list items.
   * @param <T> Helps to ensure type safety is kept when passing in the given object.
   * @return A list of generated {@link TaskListSectionView} objects.
   */
  public static <T> List<TaskListSectionView> createSectionViews(Collection<? extends TaskListSection<T>> taskListSections,
                                                                 Collection<? extends TaskListItem<T>> taskListItems,
                                                                 T target) {
    return taskListSections
        .stream()
        .map(taskListSection -> createSectionView(taskListSection, taskListItems, target))
        // Hide sections without any items
        .filter(taskListSectionView -> !taskListSectionView.taskListItemViews().isEmpty())
        .sorted(Comparator.comparing(TaskListSectionView::displayOrder))
        .toList();
  }

  public static <T> TaskListItemView createTaskListItemView(TaskListItem<T> taskListItem, T target) {
    return new TaskListItemView.TaskListItemViewBuilder(
        taskListItem.getDisplayOrder(),
        taskListItem.getItemDisplayText(),
        taskListItem.getActionUrl(target)
    )
        .withTaskListLabels(taskListItem.showTaskListLabels(target))
        .withItemValid(taskListItem.isValid(target))
        .withNotCompletedLabel(taskListItem.showNotCompletedLabels(target))
        .withCustomTaskListLabel(taskListItem.getCustomTaskListLabel(target))
        .build();
  }

  private static <T> TaskListSectionView createTaskListSectionView(TaskListSection<T> section,
                                                                   List<TaskListItemView> taskListItemViews) {
    return new TaskListSectionView(
        section.getDisplayOrder(),
        section.getSectionName(),
        section.getSectionWarningText(),
        taskListItemViews
    );
  }

}
