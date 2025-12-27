package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Table(name = "PDT_REPORT_PRODUCT", uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, ReportProduct.COLUMN_MEMBER_ID,
        ReportProduct.COLUMN_ITEM_SKU, ReportProduct.COLUMN_REASON})})
@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ReportProduct extends GdnBaseEntity {

  private static final long serialVersionUID = 5995642675411635218L;
  public static final String COLUMN_MEMBER_ID = "MEMBER_ID";
  public static final String COLUMN_ITEM_SKU = "ITEM_SKU";
  public static final String COLUMN_REASON = "REASON";
  public static final String COLUMN_NOTES = "NOTES";

  @Column(name = COLUMN_MEMBER_ID, nullable = false)
  private String memberId;

  @Column(name = COLUMN_ITEM_SKU, nullable = false)
  private String itemSku;

  @Column(name = COLUMN_REASON)
  private String reason;

  @Column(name = COLUMN_NOTES)
  private String notes;
}
