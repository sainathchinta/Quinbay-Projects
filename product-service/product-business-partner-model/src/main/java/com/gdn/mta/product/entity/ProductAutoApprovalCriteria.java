package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;
import com.gdn.mta.product.enums.AutoApprovalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = ProductAutoApprovalCriteria.TABLE_NAME)
public class ProductAutoApprovalCriteria extends GdnBaseEntity {

  private static final long serialVersionUID = -6169543984089125937L;
  public static final String TABLE_NAME = "PRD_PRODUCT_AUTO_APPROVAL_CRITERIA";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_AUTO_APPROVAL_RESPONSE = "AUTO_APPROVAL_RESPONSE";
  public static final String COLUMN_RULE_NAME = "RULE_NAME";
  public static final String COLUMN_RULE_CRITERIA = "RULE_CRITERIA";
  public static final String COLUMN_IMAGE_QC_CRITERIA = "IMAGE_QC_CRITERIA";
  public static final String COLUMN_APPROVAL_TYPE = "APPROVAL_TYPE";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_AUTO_APPROVAL_RESPONSE, nullable = false)
  private String autoApprovalResponse;

  @Column(name = COLUMN_RULE_NAME, nullable = false)
  private String ruleName;

  @Column(name = COLUMN_RULE_CRITERIA, nullable = false)
  private String ruleCriteria;

  @Column(name = COLUMN_IMAGE_QC_CRITERIA, nullable = false)
  private String imageQcCriteria;

  @Column(name = COLUMN_APPROVAL_TYPE, nullable = false)
  @Enumerated(EnumType.STRING)
  private AutoApprovalType approvalType;

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductAutoApprovalCriteria [productCode=").append(getProductCode())
        .append(", autoApprovalResponse=").append(getAutoApprovalResponse())
        .append(", ruleName=").append(getRuleName())
        .append(", ruleCriteria=").append(getRuleCriteria())
        .append(", imageQcCriteria=").append(getImageQcCriteria())
        .append(", approvalType=").append(getApprovalType())
        .append("]");
    return builder.toString();
  }

}
