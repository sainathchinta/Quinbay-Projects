package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = IPRHistory.TABLE_NAME)
public class IPRHistory extends GdnBaseEntity {
  public static final String TABLE_NAME = "PDT_IPR_HISTORY";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  public static final String COLUMN_ACTIVITY = "ACTIVITY";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";

  @Column(name = COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;
  @Column(name = COLUMN_ACTIVITY, nullable = false)
  private String activity;
  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;
}
