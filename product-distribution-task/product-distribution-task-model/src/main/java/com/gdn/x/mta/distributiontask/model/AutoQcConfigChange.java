package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Table(name = AutoQcConfigChange.TABLE_NAME)
@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AutoQcConfigChange extends GdnBaseEntity {
  public static final String TABLE_NAME = "PDT_AUTO_QC_CONFIG_CHANGE";
  public static final String SELLER_CODE = "SELLER_CODE";
  public static final String C1_CATEGORY_CODE = "C1_CATEGORY_CODE ";
  public static final String CHANGED_FIELDS = "CHANGED_FIELDS";
  public static final String NEW_SELLER = "NEW_SELLER";
  public static final String STATUS = "STATUS";

  @Column(name = SELLER_CODE)
  private String sellerCode;

  @Column(name = C1_CATEGORY_CODE)
  private String c1CategoryCode;

  @Column(name = CHANGED_FIELDS)
  private String changedFields;

  @Column(name = NEW_SELLER)
  private boolean newSeller;

  @Column(name = STATUS)
  private String status;

}
