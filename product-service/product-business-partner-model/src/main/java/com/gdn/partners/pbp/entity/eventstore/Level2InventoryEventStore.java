package com.gdn.partners.pbp.entity.eventstore;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

import com.gdn.partners.pbp.model.InventoryEventStatus;
import com.gdn.partners.pbp.commons.util.CommonUtils;

@Builder
@Entity
@Table(name = Level2InventoryEventStore.SUB_TABLE_NAME)
@PrimaryKeyJoinColumn(name = EventStore.COLUMN_ID)
@NoArgsConstructor
@AllArgsConstructor
public class Level2InventoryEventStore extends EventStore {
  public static final String SUB_TABLE_NAME = "level2_inventory_event_store";
  public static final String COLUMN_LEVEL_2_ID = "level2_id";
  public static final String COLUMN_MERCHANT_CODE = "merchant_code";
  public static final String COLUMN_INVENTORY_STATUS = "inventory_status";
  public static final String COLUMN_AVAILABLE_STOCK = "available_stock";
  public static final String COLUMN_MINIMUM_STOCK = "minimum_stock";

  @Column(name = Level2InventoryEventStore.COLUMN_LEVEL_2_ID, nullable = false)
  private String level2Id;

  @Column(name = Level2InventoryEventStore.COLUMN_MERCHANT_CODE, nullable = false)
  private String merchantCode;

  @Column(name = Level2InventoryEventStore.COLUMN_INVENTORY_STATUS, nullable = false)
  @Enumerated(EnumType.STRING)
  private InventoryEventStatus inventoryStatus;

  public String getLevel2Id() {
    return level2Id;
  }

  public void setLevel2Id(String level2Id) {
    this.level2Id = level2Id;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public InventoryEventStatus getInventoryStatus() {
    return inventoryStatus;
  }

  public void setInventoryStatus(InventoryEventStatus inventoryStatus) {
    this.inventoryStatus = inventoryStatus;
  }

  @Override
  public String toString() {
    return CommonUtils.stringifyBean(this);
  }
}
