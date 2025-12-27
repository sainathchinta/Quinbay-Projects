package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddEditedProductToPDTEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 2287790684841716237L;
  private String productCode;
  private String storeId;
  private String reviewTypes;
  private boolean postLive;
  private String merchantCode;
  private String merchantName;
  private String updatedBy;
  private boolean restrictedKeywordsPresent;
  private List<RestrictedKeywordsByFieldResponse> restrictedKeywordsDetected;
  private List<String> allModifiedFields;
  private boolean onlyImageQcDataUpdate = false;
  private ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent;
  private boolean trustedSeller;
  private String sellerBadge;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private List<PriceInfoDTO> priceInfo = new ArrayList<>();
  private boolean isPriceChange = false;
  private int distributionMappingStatus;
  private String productCreationType;
  private String brand;
  private String brandCode;
}