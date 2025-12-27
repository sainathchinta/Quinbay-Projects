package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

import java.util.List;
import java.util.Set;

public interface ExcelEditHelperService {

  /**
   * @param profileResponse
   * @param inReviewBrands
   * @param unifiedTemplateFile
   * @param isGlobalFlag
   * @param lastDownloadStatus
   * @param isAppendInReviewBrand
   * @param pickupPointNameConcat
   * @param ppNameDelimitter
   * @param shippingTypeEligibility
   * @param bulkInternalProcessType
   * @param pickupPointCodes
   * @return
   */
  UnifiedBulkDownloadDTO getProductUnifiedTemplate(ProfileResponse profileResponse,
    List<PredefinedAllowedAttributeValueResponse> inReviewBrands,
    String unifiedTemplateFile, boolean isGlobalFlag, UnifiedBulkDownloadEvent lastDownloadStatus,
    boolean isAppendInReviewBrand, boolean pickupPointNameConcat, String ppNameDelimitter,
    ShippingTypeEligibility shippingTypeEligibility, BulkInternalProcessType bulkInternalProcessType,
    Set<String> pickupPointCodes) throws Exception;
}
