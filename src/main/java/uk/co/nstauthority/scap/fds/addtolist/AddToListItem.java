package uk.co.nstauthority.scap.fds.addtolist;

public class AddToListItem {

  private final String id;
  private final String name;
  private final Boolean valid;

  public AddToListItem(String id, String name, Boolean valid) {
    this.id = id;
    this.name = name;
    this.valid = valid;
  }

  public String getId() {
    return id;
  }

  public String getName() {
    return name;
  }

  public Boolean isValid() {
    return valid;
  }
}
