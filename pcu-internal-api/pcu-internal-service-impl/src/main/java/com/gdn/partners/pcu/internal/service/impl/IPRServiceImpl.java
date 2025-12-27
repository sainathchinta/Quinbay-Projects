package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressWebResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.IPRService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.IPRProductsDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.IPRProductsDownloadWebRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@Service
public class IPRServiceImpl implements IPRService {

  @Autowired
  private PDTFeign pdtFeign;

  @Autowired
  private BPService bpService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private PartnersEngineService partnersEngineService;

  @Value("${pdp.link.product.sku.prefix}")
  private String pdpLinkProductSkuPrefix;

  @Value("${ipr.role.code.reviewer}")
  private String iprRoleCodeReviewer;

  @Override
  public Page<IPRProductListResponse> getIPRProductList(int page, int size,
      IPRProductListRequest iprProductListRequest) {
    GdnRestListResponse<IPRProductListResponse> response =
        this.pdtFeign.getIPRProductList(page, size, iprProductListRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    List<String> businessPartnerCodes =
        response.getContent().stream().map(IPRProductListResponse::getBusinessPartnerCode)
            .filter(Objects::nonNull).distinct().collect(Collectors.toList());
    Map<String, ProfileResponse> profileResponseMap =
        bpService.getProfileResponseMap(businessPartnerCodes);
    ResponseHelper.mapToIPRProductWebResponse(response.getContent(), profileResponseMap,
        pdpLinkProductSkuPrefix);
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<IprSuspensionInProgressWebResponse> getSuspensionInProcessProducts(int page, int size,
      String businessPartnerCode, String sortOrder) {
    List<IprSuspensionInProgressWebResponse> iprSuspensionInProgressWebResponses =
        new ArrayList<>();
    GdnRestListResponse<IprSuspensionInProgressResponse> response =
        pdtFeign.findSuspensionInProgressProduct(page, size, businessPartnerCode, sortOrder);
    ResponseHelper.validateMasterSkuResponse(response);
    ResponseHelper.populatePdpRedirectionUrl(response.getContent(), pdpLinkProductSkuPrefix);
    for (IprSuspensionInProgressResponse iprSuspensionInProgressResponse : response.getContent()) {
      IprSuspensionInProgressWebResponse iprSuspensionInProgressWebResponse =
          new IprSuspensionInProgressWebResponse();
      BeanUtils.copyProperties(iprSuspensionInProgressResponse, iprSuspensionInProgressWebResponse);
      iprSuspensionInProgressWebResponse.setEvidenceFilePath(String.join(Constants.COMMA_SEPARATOR,
          iprSuspensionInProgressResponse.getEvidenceFilePath()));
      iprSuspensionInProgressWebResponse.setEvidenceUrl(
          String.join(Constants.COMMA_SEPARATOR, iprSuspensionInProgressResponse.getEvidenceUrl()));
      iprSuspensionInProgressWebResponses.add(iprSuspensionInProgressWebResponse);
    }
    return new PageImpl<>(iprSuspensionInProgressWebResponses, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<String> getIPRReviewers() throws Exception {
    return partnersEngineService.getIPRReviewersByRoleCode(iprRoleCodeReviewer);
  }

  @Override
  public void updateAssignee(IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) throws Exception {
    if (StringUtils.isNotBlank(iprUpdateAssigneeRequest.getAssignedTo())) {
      validateUserNameAssigned(iprUpdateAssigneeRequest.getAssignedTo());
    }
    GdnBaseRestResponse response = pdtFeign.updateAssignee(iprUpdateAssigneeRequest);
    ResponseHelper.validateErrorCodeResponse(response);
  }

  private void validateUserNameAssigned(String userName) throws Exception {
    List<String> usernameList =
        partnersEngineService.getIPRReviewersByRoleCode(iprRoleCodeReviewer);
    if (!usernameList.contains(userName)) {
      throw new InvalidStateException(ErrorMessages.INVALID_USER_NAME);
    }
  }

  @Override
  public IPRProductDetailResponse getIPRProductDetailByProductSku(String productSku) {
    GdnRestSingleResponse<IPRProductDetailResponse> response =
        pdtFeign.getIPRProductDetail(productSku);
    ResponseHelper.validateErrorCodeResponse(response);
    ProfileResponse profileResponse = bpService.getProfileResponseByBusinessPartnerCode(
        response.getValue().getBusinessPartnerCode());
    String bpConstant = Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
        .map(CompanyDTO::getOfficer).orElse(StringUtils.EMPTY);
    boolean isOfficial = Optional.ofNullable(profileResponse).map(ProfileResponse::isOfficial)
       .orElse(Boolean.FALSE);
    StringBuilder stringBuilder = new StringBuilder();
    response.getValue().setPdpRedirectionUrl(
        stringBuilder.append(pdpLinkProductSkuPrefix).append(productSku).toString());
    response.getValue().setBpContact(bpConstant);
    response.getValue().setOfficial(isOfficial);
    return response.getValue();
  }

  @Override
  public MapResponse getPrimaryFilterCounts() {
    GdnRestSingleResponse<MapResponse> primaryFilterCountsResponse =
      pdtFeign.getPrimaryFilterCounts();
    ResponseHelper.validateMasterSkuResponse(primaryFilterCountsResponse);
    return primaryFilterCountsResponse.getValue();
  }

  @Override
  public GdnBaseRestResponse performIprAction(IprActionRequest iprActionRequest) {
    GdnBaseRestResponse response = pdtFeign.performIprAction(iprActionRequest);
    ResponseHelper.validateErrorCodeResponse(response);
    if (StringUtils.isNotBlank(response.getErrorMessage())) {
      response.setSuccess(false);
    }
    return response;
  }

  @Override
  public Page<IPRProductHistoryResponse> fetchIprProductHistory(int page, int size, String productSku) {
    GdnRestListResponse<IPRProductHistoryResponse> iprProductHistoryResponse =
        pdtFeign.fetchIprHistory(page, size, productSku);
    ResponseHelper.validateMasterSkuResponse(iprProductHistoryResponse);
    return new PageImpl<>(iprProductHistoryResponse.getContent(), PageRequest.of(page, size),
        iprProductHistoryResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public void downloadIPRProducts(String username, IPRProductsDownloadWebRequest request) {
    String requestId = UUID.randomUUID().toString();
    log.info("Invoking IPR products download with requestId: {},", requestId);
    String fileName = requestId + Constants.DOT + FileType.XLSX.name().toLowerCase();
    if (CollectionUtils.isEmpty(request.getProductSkuList())) {
      IPRProductsDownloadRequest iprProductsDownloadAllRequest =
          RequestHelper.toIPRProductsAllDownloadRequest(username, request, fileName,
              requestId);
      this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
          iprProductsDownloadAllRequest);
    } else {
      IPRProductsDownloadRequest iprProductsDownloadSelectedRequest =
          RequestHelper.toIPRProductsSelectedDownloadRequest(username, request, fileName,
              requestId);
      this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
          iprProductsDownloadSelectedRequest);
    }
  }
}
