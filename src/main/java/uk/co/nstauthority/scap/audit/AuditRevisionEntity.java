package uk.co.nstauthority.scap.audit;

import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import org.hibernate.envers.RevisionEntity;
import org.hibernate.envers.RevisionNumber;
import org.hibernate.envers.RevisionTimestamp;

@Entity
@Table(name = "audit_revisions")
@RevisionEntity(AuditRevisionEntityListener.class)
class AuditRevisionEntity {

  @Id
  @RevisionNumber
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Column(name = "rev")
  private long id;

  @RevisionTimestamp
  private Date createdTimestamp;

  private long webUserAccountId;

  long getId() {
    return id;
  }

  void setId(long id) {
    this.id = id;
  }

  Date getCreatedTimestamp() {
    return createdTimestamp;
  }

  void setCreatedTimestamp(Date createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }

  public long getWebUserAccountId() {
    return webUserAccountId;
  }

  public void setWebUserAccountId(long webUserAccountId) {
    this.webUserAccountId = webUserAccountId;
  }
}