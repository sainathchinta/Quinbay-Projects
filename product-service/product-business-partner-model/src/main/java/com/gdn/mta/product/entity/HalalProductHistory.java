package com.gdn.mta.product.entity;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.Table;

import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.gdn.GdnBaseEntity;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Entity
@EntityListeners({AuditingEntityListener.class})
@Table(name = HalalProductHistory.TABLE_NAME)
public class HalalProductHistory extends GdnBaseEntity implements Serializable {
  private static final long serialVersionUID = 8787931599551224081L;

  public static final String TABLE_NAME = "PRD_HALAL_PRODUCT_HISTORY";
  private static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  private static final String COLUMN_ACTIVITY = "ACTIVITY";
  private static final String COLUMN_CURRENT_VALUE = "CURRENT_VALUE";
  private static final String COLUMN_PREVIOUS_VALUE = "PREVIOUS_VALUE";

  @Column(name = HalalProductHistory.COLUMN_PRODUCT_SKU)
  private String productSku;

  @Column(name = HalalProductHistory.COLUMN_ACTIVITY)
  private String activity;

  @Column(name = HalalProductHistory.COLUMN_PREVIOUS_VALUE)
  private String previousValue;

  @Column(name = HalalProductHistory.COLUMN_CURRENT_VALUE)
  private String currentValue;
}
