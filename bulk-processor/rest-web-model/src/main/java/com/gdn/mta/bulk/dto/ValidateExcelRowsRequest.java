package com.gdn.mta.bulk.dto;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ValidateExcelRowsRequest {

  private Map<String, Object> raw = new HashMap<>();
  private BulkProcessNotes bulkProcessNotes;
  private BulkUploadErrorCounter bulkUploadErrorCounter;
  private boolean isInternationalMerchant;
  private Integer minimumPrice;
  private long maxStockLimit;
  private MerchantStatusType merchantStatusType;
  private int productBundlingMaxNumberOfSkus;
  private boolean productBundlingEnabled;
  private String merchantType;
  private String productBundlingEligibleMerchantTypes;
  private String commonImageErrorMessage;
  private boolean bopisCncRestrictionEnabled;
  private String primaryIdentifier;
  private boolean instoreSeller;
  private boolean pureInstoreProduct;
  private boolean sellerBopisEligible;
  private boolean categoryBopisEligible;
  private boolean overrideEmptyProductTypeBulkCreation;
  private String bopisCategoryValidationForSellerTypes;
}
