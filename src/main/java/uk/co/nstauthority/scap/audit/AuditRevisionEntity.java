package uk.co.nstauthority.scap.audit;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.Date;
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

  private Long webUserAccountId;

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

  public Long getWebUserAccountId() {
    return webUserAccountId;
  }

  public void setWebUserAccountId(Long webUserAccountId) {
    this.webUserAccountId = webUserAccountId;
  }
}
