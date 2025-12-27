package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ScreeningProductApprovalEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -5556988863347542355L;
  private String productCode;
  private String merchantCode;
  private String merchantName;
  private String updatedBy;
  private boolean postLive;
  private boolean restrictedKeywordsPresent;
  private boolean imageQcCheck;
  private List<RestrictedKeywordsByFieldResponse> restrictedKeywordsDetected;
  private boolean trustedSeller;
  private String sellerBadge;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private List<PriceInfoDTO> priceInfo = new ArrayList<>();
  private int distributionMappingStatus;
  private String productCreationType;
}
