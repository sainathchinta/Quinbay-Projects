package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductIPR.TABLE_NAME, uniqueConstraints = {
  @UniqueConstraint(columnNames = {ProductIPR.COLUMN_PRODUCT_SKU})})
public class ProductIPR extends GdnBaseEntity {

  private static final long serialVersionUID = -2222382112406616778L;
  public static final String TABLE_NAME = "PDT_PRODUCT_IPR";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_PRODUCT_NAME = "PRODUCT_NAME";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  public static final String COLUMN_BRAND_NAME = "BRAND_NAME";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_BUSINESS_PARTNER_NAME = "BUSINESS_PARTNER_NAME";
  public static final String COLUMN_BRAND_CODE = "BRAND_CODE";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_EVIDENCE_FILE_PATH = "EVIDENCE_FILE_PATH";
  public static final String COLUMN_EVIDENCE_URL = "EVIDENCE_URL";
  public static final String COLUMN_EVIDENCE_SUBMITTED_NOTES = "EVIDENCE_SUBMITTED_NOTES";
  public static final String COLUMN_EVIDENCE_SUBMITTED_BY = "EVIDENCE_SUBMITTED_BY";
  public static final String COLUMN_REASONS = "REASONS";
  public static final String COLUMN_SELLER_NOTES = "SELLER_NOTES";
  public static final String COLUMN_EVIDENCE_REQUESTED_DATE = "EVIDENCE_REQUESTED_DATE";
  public static final String COLUMN_EVIDENCE_REQUESTED_BY = "EVIDENCE_REQUESTED_BY";
  public static final String COLUMN_PRODUCT_ADDED_DATE = "PRODUCT_ADDED_DATE";
  public static final String COLUMN_ASSIGNED_TO = "ASSIGNED_TO";
  public static final String COLUMN_ASSIGNED_DATE = "ASSIGNED_DATE";
  public static final String COLUMN_IMAGE_URL = "IMAGE_URL";
  public static final String COLUMN_VIOLATION_TYPE = "VIOLATION_TYPE";
  public static final String COLUMN_SOURCE = "SOURCE";
  public static final String COLUMN_REVIEWER_NOTES = "REVIEWER_NOTES";
  public static final String COLUMN_VIOLATED_FIELDS = "VIOLATED_FIELDS";
  public static final String COLUMN_REPORT_DATE = "REPORT_DATE";
  public static final String COLUMN_REPORTER = "REPORTER";
  public static final String COLUMN_REPORTER_NAME = "REPORTER_NAME";
  public static final String COLUMN_REPORTER_EMAIL = "REPORTER_EMAIL";
  public static final String COLUMN_REPORTER_REASON = "REPORTER_REASON";
  public static final String COLUMN_REPORTER_ADDRESS = "REPORTER_ADDRESS";
  public static final String COLUMN_REPORTER_PHONE_NUMBER = "REPORTER_PHONE_NUMBER";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_PRODUCT_NAME, nullable = false)
  private String productName;

  @Column(name = COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;

  @Column(name = COLUMN_BRAND_CODE, nullable = false)
  private String brandCode;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = COLUMN_BUSINESS_PARTNER_NAME, nullable = false)
  private String businessPartnerName;

  @Column(name = COLUMN_BRAND_NAME, nullable = false)
  private String brandName;

  @Column(name = COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

  @Column(name = COLUMN_CATEGORY_NAME, nullable = false)
  private String categoryName;

  @Column(name = COLUMN_STATE, nullable = false)
  private String state;

  @Column(name = COLUMN_PRODUCT_ADDED_DATE, nullable = false)
  private Date productAddedDate;

  @Column(name = COLUMN_EVIDENCE_FILE_PATH)
  private String evidenceFilePath;

  @Column(name = COLUMN_EVIDENCE_URL)
  private String evidenceUrl;

  @Column(name = COLUMN_EVIDENCE_SUBMITTED_NOTES)
  private String evidenceSubmittedNotes;

  @Column(name = COLUMN_EVIDENCE_SUBMITTED_BY)
  private String evidenceSubmittedBy;

  @Column(name = COLUMN_REASONS)
  private String reasons;

  @Column(name = COLUMN_SELLER_NOTES)
  private String sellerNotes;

  @Column(name = COLUMN_EVIDENCE_REQUESTED_DATE)
  private Date evidenceRequestedDate;

  @Column(name = COLUMN_EVIDENCE_REQUESTED_BY)
  private String evidenceRequestedBy;

  @Column(name = COLUMN_ASSIGNED_TO)
  private String assignedTo;

  @Column(name = COLUMN_ASSIGNED_DATE)
  private Date assignedDate;

  @Column(name = COLUMN_IMAGE_URL)
  private String imageUrl;

  @Column(name = COLUMN_VIOLATION_TYPE)
  private String violationType;

  @Column(name = COLUMN_SOURCE)
  private String source;

  @Column(name = COLUMN_REVIEWER_NOTES)
  private String reviewerNotes;

  @Column(name = COLUMN_VIOLATED_FIELDS)
  private String violatedFields;

  @Column(name = COLUMN_REPORT_DATE)
  private Date reportDate;

  @Column(name = COLUMN_REPORTER)
  private String reporter;

  @Column(name = COLUMN_REPORTER_NAME)
  private String reporterName;

  @Column(name = COLUMN_REPORTER_EMAIL)
  private String reporterEmail;

  @Column(name = COLUMN_REPORTER_REASON)
  private String reporterReason;

  @Column(name = COLUMN_REPORTER_ADDRESS)
  private String reporterAddress;

  @Column(name = COLUMN_REPORTER_PHONE_NUMBER)
  private String reporterPhoneNumber;
}
