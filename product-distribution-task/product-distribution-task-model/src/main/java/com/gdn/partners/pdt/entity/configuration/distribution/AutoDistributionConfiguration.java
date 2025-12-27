package com.gdn.partners.pdt.entity.configuration.distribution;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;

import com.gdn.partners.pdt.model.configuration.distribution.PriorityType;

import lombok.Data;

@Data
@Entity
@Table(name = AutoDistributionConfiguration.TABLE_NAME)
public class AutoDistributionConfiguration {

  private static final long serialVersionUID = 3183793383028596808L;
  public static final String TABLE_NAME = "PDT_AUTO_DISTRIBUTION_CONFIGURATION";
  public static final String COLUMN_VENDOR_CODE = "VENDOR_CODE";
  public static final String COLUMN_PRIORITY = "PRIORITY";
  public static final String COLUMN_PRIORITY_TYPE = "PRIORITY_TYPE";
  public static final String COLUMN_PRIORITY_VALUE = "PRIORITY_VALUE";
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

  @Column(name = AutoDistributionConfiguration.COLUMN_VENDOR_CODE, nullable = false)
  private String vendorCode;

  @Column(name = AutoDistributionConfiguration.COLUMN_PRIORITY, nullable = false)
  private Integer priority;

  @Enumerated(EnumType.STRING)
  @Column(name = AutoDistributionConfiguration.COLUMN_PRIORITY_TYPE, nullable = false)
  private PriorityType priorityType;

  @Column(name = AutoDistributionConfiguration.COLUMN_PRIORITY_VALUE, nullable = false)
  private String priorityValue;

  public AutoDistributionConfiguration() {}

  public AutoDistributionConfiguration(String vendorCode, Integer priority, PriorityType priorityType,
      String priorityValue) {
    super();
    this.vendorCode = vendorCode;
    this.priority = priority;
    this.priorityType = priorityType;
    this.priorityValue = priorityValue;
  }
}
