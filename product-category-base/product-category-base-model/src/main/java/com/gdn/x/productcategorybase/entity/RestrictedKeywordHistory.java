package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Entity
@EntityListeners({AuditingEntityListener.class})
@Table(name = RestrictedKeywordHistory.TABLE_NAME)
public class RestrictedKeywordHistory implements Serializable {
  private static final long serialVersionUID = 8787931599551224081L;

  public static final String TABLE_NAME = "PCC_RESTRICTED_KEYWORD_HISTORY";
  public static final String ID = "ID";
  public static final String CREATED_BY = "CREATED_BY";
  public static final String CREATED_DATE = "CREATED_DATE";
  public static final String MARK_FOR_DELETE = "MARK_FOR_DELETE";
  public static final String STORE_ID = "STORE_ID";
  public static final String UPDATED_BY = "UPDATED_BY";
  public static final String UPDATED_DATE = "UPDATED_DATE";
  private static final String COLUMN_KEYWORD_ID = "KEYWORD_ID";
  private static final String COLUMN_ACTIVITY = "ACTIVITY";
  private static final String COLUMN_OLD_VALUE = "OLD_VALUE";
  private static final String COLUMN_NEW_VALUE = "NEW_VALUE";

  @Id
  @Column(name = ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  private String id;

  @Column(name = STORE_ID)
  private String storeId;

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

  @Column(name = RestrictedKeywordHistory.COLUMN_KEYWORD_ID)
  private String keywordId;

  @Column(name = RestrictedKeywordHistory.COLUMN_ACTIVITY)
  private String activity;

  @Column(name = RestrictedKeywordHistory.COLUMN_OLD_VALUE)
  private String oldValue;

  @Column(name = RestrictedKeywordHistory.COLUMN_NEW_VALUE)
  private String newValue;
}
