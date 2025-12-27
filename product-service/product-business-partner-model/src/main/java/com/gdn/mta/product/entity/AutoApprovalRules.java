package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;
import com.gdn.mta.product.enums.AutoApprovalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = AutoApprovalRules.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, AutoApprovalRules.COLUMN_SEQUENCE_NUMBER}),
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, AutoApprovalRules.COLUMN_RULE_NAME})})
public class AutoApprovalRules extends GdnBaseEntity {

  private static final long serialVersionUID = 3014234306146874576L;
  public static final String TABLE_NAME = "PRD_AUTO_APPROVAL_RULES";
  public static final String COLUMN_SEQUENCE_NUMBER = "SEQUENCE_NUMBER";
  public static final String COLUMN_RULE_NAME = "RULE_NAME";
  public static final String COLUMN_RULE_CONFIG = "RULE_CONFIG";
  public static final String COLUMN_IMAGE_QC_CONFIG = "IMAGE_QC_CONFIG";
  public static final String COLUMN_NEED_REVISION_CONFIG = "NEED_REVISION_CONFIG";
  public static final String COLUMN_NEED_REVISION_ENABLED = "NEED_REVISION_ENABLED";
  public static final String COLUMN_AUTO_APPROVAL_TYPE = "AUTO_APPROVAL_TYPE";

  @Column(name = COLUMN_SEQUENCE_NUMBER, nullable = false)
  private int sequenceNumber;

  @Column(name = COLUMN_RULE_NAME, nullable = false)
  private String ruleName;

  @Column(name = COLUMN_RULE_CONFIG, nullable = false)
  private String ruleConfig;

  @Column(name = COLUMN_IMAGE_QC_CONFIG, nullable = false)
  private String imageQcConfig;

  @Column(name = COLUMN_NEED_REVISION_ENABLED, nullable = false)
  private boolean needRevisionEnabled;

  @Column(name = COLUMN_NEED_REVISION_CONFIG, nullable = false)
  private String needRevisionConfig;

  @Column(name = COLUMN_AUTO_APPROVAL_TYPE, nullable = false)
  @Enumerated(EnumType.STRING)
  private AutoApprovalType autoApprovalType;

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("AutoApprovalRules [sequenceNumber=").append(getSequenceNumber())
        .append(", ruleName=").append(getRuleName())
        .append(", ruleConfig=").append(getRuleConfig())
        .append(", imageQcConfig=").append(getImageQcConfig())
        .append(", autoApprovalType=").append(getAutoApprovalType())
        .append("]");
    return builder.toString();
  }
}
