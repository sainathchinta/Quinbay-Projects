package com.gdn.x.mta.distributiontask.model;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.Version;

import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@MappedSuperclass
@NoArgsConstructor
@AllArgsConstructor
@EntityListeners(value = {AuditingEntityListener.class})
public abstract class GdnBaseEntity implements Serializable {
  private static final long serialVersionUID = -2926245364422822164L;

  public static final String CREATED_BY = "CREATED_BY";
  public static final String CREATED_DATE = "CREATED_DATE";
  public static final String ID = "ID";
  public static final String MARK_FOR_DELETE = "MARK_FOR_DELETE";
  public static final String OPTLOCK = "OPTLOCK";
  public static final String STORE_ID = "STORE_ID";
  public static final String UPDATED_BY = "UPDATED_BY";
  public static final String UPDATED_DATE = "UPDATED_DATE";

  @Id
  @Column(name = ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  private String id;

  @Column(name = STORE_ID)
  private String storeId;

  @Version
  @Column(name = OPTLOCK)
  private Long version;

  @CreatedDate
  @Column(name = CREATED_DATE, nullable = false)
  private Date createdDate;

  @CreatedBy
  @Column(name = CREATED_BY)
  private String createdBy;

  @LastModifiedDate
  @Column(name = UPDATED_DATE)
  private Date updatedDate;

  @LastModifiedBy
  @Column(name = UPDATED_BY)
  private String updatedBy;

  @Column(name = MARK_FOR_DELETE)
  private boolean markForDelete;

}
