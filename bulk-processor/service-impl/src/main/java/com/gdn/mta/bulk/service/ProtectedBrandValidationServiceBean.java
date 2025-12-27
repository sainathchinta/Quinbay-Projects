package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.GenericBulkHeaders;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;

@Service
@Transactional(readOnly = true)
@Slf4j
public class ProtectedBrandValidationServiceBean implements ProtectedBrandValidationService {

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Override
  public boolean validateProtectedBrandAuthorisation(String brandName, BulkProcess bulkProcess,
    Map<String, String> protectedBrandNameCodeMap) {
    String channelId = MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    String clientId = MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    String username = MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    if (protectedBrandNameCodeMap.containsKey(brandName)) {
      SimpleBooleanResponse simpleBooleanResponse = pcbOutboundService
        .getBrandAuthorisation(bulkProcess.getStoreId(), channelId, clientId,
          bulkProcess.getRequestId(), username, bulkProcess.getBusinessPartnerCode(),
          protectedBrandNameCodeMap.get(brandName));
      return simpleBooleanResponse.getResult();
    }
    return true;
  }

  @Override
  public Map<String, String> fetchProtectedBrandNameCodeMap(String storeId) {
    List<ProtectedBrandResponse> protectedBrandList =
      pcbOutboundService.getProtectedBrandList(storeId);
    return BulkCreationCommonUtil.prepareProtectedBrandNameCodeMap(protectedBrandList);
  }

  @Override
  public boolean validateProtectedBrand(Map<String, Object> row, BulkProcess bulkProcess,
      Map<String, String> protectedBrandNameCodeMap) {
    String brandValue = String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, GenericBulkHeaders.BRAND, GenericBulkHeaders.BRAND_EN));
    String cleanBrandValue = brandValue.contains(Constant.IN_REVIEW) ?
        brandValue.substring(0, brandValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX)) :
        brandValue;
    if (StringUtils.isNotBlank(cleanBrandValue)) {
      return validateProtectedBrandAuthorisation(cleanBrandValue, bulkProcess, protectedBrandNameCodeMap);
    }
    //if clean brand value is empty or blank then directly sending it as true.
    return true;
  }

  @Override
  public boolean validateProtectedBrandForCn(Map<String, Object> row, BulkProcess bulkProcess,
      Map<String, String> protectedBrandNameCodeMap) {
    String brandValue = String.valueOf(row.get(BulkCnCreationHeaderNames.BRAND_HEADER_NAME));
    String cleanBrandValue =
        brandValue.contains(Constant.IN_REVIEW) ?
            brandValue.substring(0, brandValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX)) :
            brandValue;
    if (StringUtils.isNotBlank(cleanBrandValue)) {
      return validateProtectedBrandAuthorisation(cleanBrandValue, bulkProcess, protectedBrandNameCodeMap);
    }
    //if clean brand value is empty or blank then directly sending it as true.
    return true;
  }
}
