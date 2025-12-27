package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Event will be published to create clone of product with existing details. This will be similar process as flow 2
 * product creation but automatically. (without search one by one and add)
 *
 * <p>
 * User input parameters for flow 2 (like selling unit price, SKU based pre-defined attributes etc.) will be taken from
 * given L2 (item code) / L4 (item SKU) details and pickup point details will be taken from target store.
 * <p>
 * This will be used for merchants which has multiple stores and want to add same products to all / some stores hassle
 * free.
 *
 * @author anand
 * @since Sep 2019
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateProductSyncEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -5495302829172843046L;

  private String storeId;

  private String username;

  private List<String> sourceItemSkus;

  private String pickupPointCode;

  private String businessPartnerCode;

}
