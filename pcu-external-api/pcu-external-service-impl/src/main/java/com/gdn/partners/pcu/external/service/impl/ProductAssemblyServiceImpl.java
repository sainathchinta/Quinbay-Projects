package com.gdn.partners.pcu.external.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.feign.ProductAssemblyFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.ProductAssemblyService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisAssemblyListingRequest;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.request.TransferRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductAssemblyServiceImpl implements ProductAssemblyService {

  @Autowired
  private ProductAssemblyFeign productAssemblyFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private XProductFeign xProductFeign;


  @Value("${master.warehouse.list.page}")
  private String page;

  @Value("${master.warehouse.list.limit}")
  private String limit;

  @Value("${bundling.allowed.seller.type}")
  private String bundlingAllowedSellerType;

  @Value("${validate.business.partner.code.for.security.enabled}")
  private boolean validateBusinessPartnerCodeForSecurityEnabled;

  @Override
  public GdnRestListResponse<MasterWarehouseListWebResponse> getMasterWarehouseListResponse() {
    return productAssemblyFeign.getWarehouseCodeAndFulfillmentCenter(page, limit);
  }

  @Override
  public GdnRestListResponse<RequestFormWebResponse> getRequestFormsListingResponse(int page, int size,
      String requestId, AssemblyDisAssemblyListingRequest request) {
    if(validateBusinessPartnerCodeForSecurityEnabled) {
      RequestHelper.validateAccessibilityForAssemblyAndDisassembly(request.getType());
    }
    GdnRestListResponse<RequestFormResponse> response = productAssemblyFeign.getListingResponse(page, size, request);
    ResponseHelper.validateResponse(response);
    GdnRestListResponse<RequestFormWebResponse> webResponse = new GdnRestListResponse<>();
    if (CollectionUtils.isNotEmpty(response.getContent())) {
      Map<String, Boolean> isBundleItem = isBundleItem(response.getContent());
      List<RequestFormWebResponse> requestFormWebResponseList =
          response.getContent().stream().map(requestFormResponse -> getWebResponse(requestFormResponse, isBundleItem))
              .collect(Collectors.toList());
      BeanUtils.copyProperties(response, webResponse);
      webResponse.setContent(requestFormWebResponseList);
      return webResponse;
    }
    return new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), requestId);
  }

  @Override
  public GdnRestListResponse<HistoryWebResponse> getRequestHistory(String requestFormNumber, int page, int size,
      String sortOrder, String requestId, String merchantCode) {
    GdnRestListResponse<HistoryWebResponse> response = productAssemblyFeign.getHistory(page, size, sortOrder, requestFormNumber, merchantCode);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse cancelOrRetry(String requestFormNumber, String type, String merchantCode, String requestId) {
    GdnBaseRestResponse response =
        productAssemblyFeign.cancelOrRetryRequestForm(requestFormNumber, type, merchantCode, requestId);
    ResponseHelper.validateResponse(response);
    return response;
  }

  private RequestFormWebResponse getWebResponse(RequestFormResponse response, Map<String, Boolean> isBundleItem) {
    GdnRestListResponse<CategoryResponse> categoryResponse =
        pcbFeign.filterCategoryHierarchyByCategoryCode(response.getCategoryCode());
    ResponseHelper.validateResponse(categoryResponse);
    RequestFormWebResponse formWebResponse = new RequestFormWebResponse();
    BeanUtils.copyProperties(response, formWebResponse);
    formWebResponse.setCategoryName(
        categoryResponse.getContent().stream().findFirst().map(CategoryResponse::getName).orElse(StringUtils.EMPTY));
    formWebResponse.setBundleProduct(isBundleItem.getOrDefault(response.getItemSku(), false));
    formWebResponse.setTransferFromBundleProduct(isBundleItem.getOrDefault(response.getTransferFromItemSku(), false));
    return formWebResponse;
  }

  @Override
  public void createAssemblyDisAssemblyAndTransferRequests(String businessPartnerCode, String type,
      SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest) {
    ProfileResponse profileResponse =
        businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    RequestHelper.validateBundlingAllowedBasedOnSellerType(profileResponse, bundlingAllowedSellerType);
    if (CollectionUtils.isNotEmpty(simpleListAssemblyDisassemblyRequest.getValue())) {
      GdnBaseRestResponse response = null;
      if (Constants.TRANSFER_REQUEST.equalsIgnoreCase(type)) {
        TransferRequest transferRequest = new TransferRequest();
        BeanUtils.copyProperties(simpleListAssemblyDisassemblyRequest.getValue().get(0),
            transferRequest);
        response = productAssemblyFeign.transferRequest(transferRequest);
      } else {
        response = productAssemblyFeign.assemblyDisassemblyRequest(type,
            simpleListAssemblyDisassemblyRequest);
      }
      ResponseHelper.validateResponse(response);
    }
  }

  private Map<String, Boolean> isBundleItem(List<RequestFormResponse> requestFormResponses) {
    List<String> itemSkus = Stream.concat(requestFormResponses.stream().map(RequestFormResponse::getItemSku),
            requestFormResponses.stream().map(RequestFormResponse::getTransferFromItemSku)).filter(Objects::nonNull)
        .distinct().collect(Collectors.toList());
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        xProductFeign.getItemBasicDetails(true, new SimpleListStringRequest(itemSkus));
    return Optional.ofNullable(response).map(GdnRestListResponse::getContent).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(ItemBasicDetailV2Response::getItemSku,
            itemBasicDetailV2Response -> CollectionUtils.isNotEmpty(itemBasicDetailV2Response.getBundleRecipeList()),
            (v1, v2) -> v2));
  }
}
