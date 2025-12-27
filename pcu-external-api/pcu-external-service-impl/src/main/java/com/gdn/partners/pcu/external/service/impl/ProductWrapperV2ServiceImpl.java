package com.gdn.partners.pcu.external.service.impl;

import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.ProductV2Service;
import com.gdn.partners.pcu.external.service.ProductWrapperV2Service;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ProductWrapperV2ServiceImpl implements ProductWrapperV2Service {

  @Autowired
  private ProductV2Service productV2Service;

  @Autowired
  private ProductService productService;

  @Autowired
  private UserPicService userPicService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${validate.business.partner.code.for.security.enabled}")
  private boolean validateBusinessPartnerCodeForSecurityEnabled;

  @Value("${validate.product.accessibility}")
  private boolean validateProductAccessibility;

  @Value("${product.accessibility.list}")
  private String productAccessibilityList;

  @Override
  public void updateItemListing(String productSku, List<QuickEditV2WebRequest> quickEditRequests)
    throws Exception {
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility,
        Arrays.asList(Credential.getAccessibilities()), Boolean.parseBoolean(mandatoryParameterHelper.isExternal()),
        productAccessibilityList, mandatoryParameterHelper.getClientType());
    RequestHelper.checkProductSkuOrItemSkuStartsWithBPCode(Collections.singletonList(productSku),
        validateBusinessPartnerCodeForSecurityEnabled, mandatoryParameterHelper.getBusinessPartnerCode());
    Set<String> requestChangesToPickupPoints =
      quickEditRequests.stream().map(QuickEditV2WebRequest::getPickupPointCode)
        .collect(Collectors.toSet());
    userPicService.validateUserPicPickupPoints(requestChangesToPickupPoints);
    if (multiPickupPointEnabled) {
      boolean isExternal = Boolean.valueOf(mandatoryParameterHelper.isExternalOnly());
      productV2Service.updateItemListing(productSku, quickEditRequests, isExternal);
    } else {
      productService.updateItemListing(productSku,
        RequestHelper.toQuickEditWebRequestList(quickEditRequests));
    }
  }

  @Override
  public Page<ProductLevel3ListingV2WebResponse> getProductL3Listing(
    ProductSummaryV2WebRequest request, Integer page, Integer size, boolean onlyDefaultViewConfig) {
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility, Arrays.asList(Credential.getAccessibilities()),
        Boolean.parseBoolean(mandatoryParameterHelper.isExternal()), productAccessibilityList,
        mandatoryParameterHelper.getClientType());
    if (StringUtils.isNotEmpty(request.getKeyword())) {
      request.setKeyword(request.getKeyword().toUpperCase());
    }
    return this.productV2Service.getProductL3List(request, page, size, onlyDefaultViewConfig);
  }
}
