package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = ProductItemSyncProcess.TABLE_NAME, uniqueConstraints = {
  @UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ProductItemSyncProcess.PROCESS_ID
  })
})
public class ProductItemSyncProcess extends GdnBaseEntity {

  public static final String TABLE_NAME = "prd_product_item_sync_process";

  public static final String PROCESS_ID = "process_id";

  public static final String IS_USER_NOTIFIED = "is_user_notified";

  @Column(name = ProductItemSyncProcess.PROCESS_ID, nullable = false)
  private String processId;

  @Column(name = ProductItemSyncProcess.IS_USER_NOTIFIED, nullable = false)
  private boolean isUserNotified;

}
