package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Slf4j
@Service
public class ExcelEditHelperServiceImpl implements ExcelEditHelperService {

  private static final String PICKUP_POINTS = "pickupPoints";
  private static final String BRAND = "Brand";

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private PickupPointService pickupPointService;

  @Value("${gcs.blibli.mass.budling.template.path}")
  private String gcsMassBundlingTemplatePath;

  @Value("${bulk.excel.versioning.switch.en}")
  private boolean bulkExcelVersioningEn;

  @Value("${bulk.generic.excel.version}")
  private String bulkGenericExcelVersion;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${bulk.generic.instore.excel.version}")
  private String bulkGenericInstoreExcelVersion;

  @Override
  public UnifiedBulkDownloadDTO getProductUnifiedTemplate(ProfileResponse profileResponse,
    List<PredefinedAllowedAttributeValueResponse> inReviewBrands, String unifiedTemplateFile,
    boolean isGlobalFlag, UnifiedBulkDownloadEvent lastDownloadStatus,
    boolean isAppendInReviewBrand, boolean pickupPointNameConcat, String ppNameDelimitter,
    ShippingTypeEligibility shippingTypeEligibility, BulkInternalProcessType bulkInternalProcessType,
    Set<String> pickupPointCodes) throws Exception {
    boolean isWhiteListedMerchant = Objects.nonNull(profileResponse.getMultiDefaultAddressFlag()) ?
      profileResponse.getMultiDefaultAddressFlag() : false;
    byte[] destinationFileByteFile;
    PickupPointFilterRequest pickupPointRequest = PickupPointFilterRequest.builder()
        .businessPartnerCode(profileResponse.getBusinessPartnerCode()).build();
    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      pickupPointRequest.setCodes(pickupPointCodes);
      Optional.ofNullable(lastDownloadStatus).ifPresent(downloadStatus -> downloadStatus.setPickupPointUpdated(true));
    }
    List<PickupPointResponse> pickupPoints = this.pickupPointService.getPickupPointSummaryFilter(0,
      pickupPointRequest);
    Map<String, List<?>> datas = new HashMap<>();
    datas.put(PICKUP_POINTS, pickupPoints);
    datas.put(BRAND, inReviewBrands);
    if (isGlobalFlag) {
      destinationFileByteFile = fileStorageService.downloadGenericTemplateFile(unifiedTemplateFile);
    } else {
      destinationFileByteFile = fileStorageService.downloadGenericTemplateFile(
          getGenericBusinessPartnerFilePath(profileResponse.getBusinessPartnerCode(),
              bulkInternalProcessType,
              CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled,
                  profileResponse)));
    }
    boolean isRegeneratePickupPointSheet =
        ExcelTemplateUtil.checkForDataListAndFlagStatus(datas.get(PICKUP_POINTS), lastDownloadStatus, PICKUP_POINTS,
            isGlobalFlag);
    boolean isRegenerateBrandSheet =
        ExcelTemplateUtil.checkForDataListAndFlagStatus(datas.get(BRAND), lastDownloadStatus, BRAND, isGlobalFlag);
    if (isRegeneratePickupPointSheet) {
      destinationFileByteFile =
          ExcelTemplateUtil.setProductUnifiedTemplatePickupPoints(datas, destinationFileByteFile, isWhiteListedMerchant,
              pickupPointNameConcat, ppNameDelimitter);
    }
    destinationFileByteFile =
      ExcelTemplateUtil.setShippingOptionInProductUnifiedTemplate(destinationFileByteFile,
        shippingTypeEligibility, profileResponse.getCompany().isInternationalFlag());
    if (isAppendInReviewBrand && isRegenerateBrandSheet) {
      destinationFileByteFile = ExcelTemplateUtil.setProductUnifiedTemplateBrandValues(datas,
        destinationFileByteFile);
    }
    String filePath =
      fileStorageService.createGenericBulkFile(profileResponse.getBusinessPartnerCode(),
        CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, profileResponse),
        bulkInternalProcessType, destinationFileByteFile);
    return UnifiedBulkDownloadDTO.builder().destinationFileByteFile(destinationFileByteFile)
      .filePath(filePath).build();
  }

  private String getGenericBusinessPartnerFilePath(String businessPartnerCode,
      BulkInternalProcessType bulkInternalProcessType, boolean instoreSeller) {
    StringBuilder filePath;
    if (BulkInternalProcessType.GENERIC_FILE_GENERATION.equals(bulkInternalProcessType)) {
      filePath = new StringBuilder(businessPartnerCode).append(Constant.SLASH).append(Constant.NEW_GENERAL_TEMPLATE);
    } else {
      filePath = new StringBuilder(gcsMassBundlingTemplatePath).append(Constant.SLASH).append(businessPartnerCode)
          .append(Constant.SLASH).append(Constant.NEW_GENERAL_TEMPLATE);
    }
    if (bulkExcelVersioningEn) {
      String version = bulkGenericExcelVersion;
      if (instoreSeller) {
        version = bulkGenericInstoreExcelVersion;
      }
      filePath.append(Constant.UNDERSCORE).append(version);
    }
    filePath.append(Constant.NEW_GENERAL_TEMPLATE_EXTENSION);
    return filePath.toString();
  }
}
