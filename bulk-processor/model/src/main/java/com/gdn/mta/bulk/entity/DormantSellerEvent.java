package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = DormantSellerEvent.TABLE_NAME)
public class DormantSellerEvent extends GdnBaseEntity {
  public static final String TABLE_NAME = "BLP_DORMANTSELLER_EVENT";

  private static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_PROCESS_TYPE = "PROCESS_TYPE";
  private static final String COLUMN_RETRY_COUNT = "RETRY_COUNT";

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_PROCESS_TYPE)
  private String processType;

  @Column(name = COLUMN_RETRY_COUNT)
  private int retryCount;
}
