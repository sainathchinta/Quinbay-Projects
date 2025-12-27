package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.MasterCatalogFeign;
import com.gdn.partners.pcu.internal.client.model.request.ChangeAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.ClusterReviewFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.AcceptRejectActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.ItemSkuListRequest;
import com.gdn.partners.pcu.internal.client.model.request.MasterSkuItemsListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.ClusterActionResponse;
import com.gdn.partners.pcu.internal.client.model.response.CompareAnchorResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuAndIndicativePriceResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.MasterSkuConfigResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.MasterSkuReviewService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuReviewAllItemsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuReviewSelectedItemsDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuItemsDownloadWebRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuInReviewDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuInReviewDownloadWebRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Navya Naveli
 */

@Service
@Slf4j
public class MasterSkuReviewServiceImpl implements MasterSkuReviewService {

  @Autowired
  private MasterCatalogFeign masterCatalogFeign;

  @Autowired
  private PartnersEngineService partnersEngineService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Value("${pdp.link.prefix}")
  private String pdpLinkPrefix;

  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public GdnRestListResponse<InReviewListResponse> getInReviewProductList(int page, int size,
      InReviewListWebRequest inReviewListWebRequest) {

    GdnRestListResponse<InReviewListResponse> response;
    response = masterCatalogFeign.getItemsForInReview(page, size, inReviewListWebRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<ItemsListingResponse> getAllMasterSkuItemsList(int page, int size,
      MasterSkuItemsListWebRequest masterSkuItemsListWebRequest) {

    GdnRestListResponse<ItemsListingResponse> response =
        masterCatalogFeign.getAllMasterSkuItemsList(page, size, masterSkuItemsListWebRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    ResponseHelper.setPdpRedirectionLink(pdpLinkPrefix, response.getContent());
    return response;
  }

  @Override
  public GdnRestSingleResponse<ItemSkuDetailResponse> getMasterSkuDetails(String masterSku,
    boolean allData) {
    GdnRestSingleResponse<ItemSkuDetailResponse> response =
      masterCatalogFeign.getMasterSkuDetails(masterSku, allData);
    ResponseHelper.validateMasterSkuResponse(response);
    ResponseHelper.setPdpRedirectionLinkForItemDetailResponses(pdpLinkPrefix,
      List.of(response.getValue()));
    return response;
  }

  @Override
  public GdnRestListResponse<ItemSkuDetailResponse> fetchItemsMappedToMasterSku(int page, int size, String masterSku) {
    GdnRestListResponse<ItemSkuDetailResponse> response =
        masterCatalogFeign.fetchItemsMappedToMasterSku(page, size, masterSku);
    ResponseHelper.validateMasterSkuResponse(response);
    ResponseHelper.setPdpRedirectionLinkForItemDetailResponses(pdpLinkPrefix, response.getContent());
    return response;
  }

  @Override
  public GdnRestSingleResponse<CompareAnchorResponse> getCompareAnchorDetails(String firstAnchor,
    String secondAnchor) {
    GdnRestSingleResponse<CompareAnchorResponse> response =
      masterCatalogFeign.getCompareAnchorSkuDetails(firstAnchor, secondAnchor);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<ItemHistoryResponse> fetchItemHistoryDetails(int page, int size, String itemSku) {
    GdnRestListResponse<ItemHistoryResponse> response = masterCatalogFeign.fetchItemHistoryDetails(page, size, itemSku);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnRestSingleResponse<MasterSkuConfigResponse> fetchMasterSkuReviewConfig() {
    GdnRestSingleResponse response = masterCatalogFeign.fetchMasterSkuReviewConfig();
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnRestSingleResponse<ClusterActionResponse> performClusterAction(String masterSku,
      ClusterReviewFeedbackRequest clusterReviewFeedbackRequest) {
    GdnRestSingleResponse<ClusterActionResponse> response =
        masterCatalogFeign.performClusterReviewAction(masterSku, clusterReviewFeedbackRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public void updateAcceptRejectActionRequest(String firstAnchorSku, String secondAnchorSku,
      AcceptRejectActionRequest request) {
    GdnBaseRestResponse response = masterCatalogFeign
        .updateAnchorsForAcceptRejectAction(firstAnchorSku, secondAnchorSku, request);
    ResponseHelper.validateMasterSkuResponse(response);
  }

  @Override
  public void updateChangeAssigneeAction(ChangeAssigneeRequest request) throws Exception {
    if (StringUtils.isNotBlank(request.getNewAssignee())) {
      validateUserNameAssigned(request.getNewAssignee());
    }
    GdnBaseRestResponse response = masterCatalogFeign.updateAssignedTo(request);
    ResponseHelper.validateMasterSkuResponse(response);
  }

  @Override
  public List<String> getMasterSkuReviewReviewers() throws Exception {
    return partnersEngineService.getMasterSkuReviewers();
  }

  @Override
  public void downloadItemsForMasterSkuReview(String username, MasterSkuItemsDownloadWebRequest request, int page,
      int size) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking  download items for master sku review. requestId: {},", requestId);
    String fileName =
        new StringBuilder().append(requestId).append(Constants.DOT).append(FileType.XLSX.name().toLowerCase())
            .toString();
    if (CollectionUtils.isEmpty(request.getItemSkuList())) {
      MasterSkuReviewAllItemsDownloadRequest masterSkuReviewAllItemsDownloadRequest =
          RequestHelper.toMasterSkuReviewAllItemsDownloadRequest(username, request, fileName, requestId, page, size);
      this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
          masterSkuReviewAllItemsDownloadRequest);
    } else {
      MasterSkuReviewSelectedItemsDownloadRequest masterSkuReviewSelectedItemsDownloadRequest =
          RequestHelper.toMasterSkuReviewSelectedItemsDownloadRequest(username, request, fileName, requestId);
      this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
          masterSkuReviewSelectedItemsDownloadRequest);
    }
  }


  @Override
  public void bulkDownloadInReviewAnchorMappings(String username,
    MasterSkuInReviewDownloadWebRequest request, int page, int size) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking of selected master products download. requestId: {},", requestId);
    MasterSkuInReviewDownloadRequest masterSkuInReviewDownloadRequest =
      RequestHelper.toMasterSkuInReviewDownloadRequest(request, username, requestId, page, size);
    this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
      masterSkuInReviewDownloadRequest);
  }

  @Override
  public void uploadBulkFile(MultipartFile file, String actionType, String requestId,
    String storeId, String username) throws Exception {
    String baseDirPath = fileStorageService.uploadFilePath(file, requestId, actionType);
    BulkReviewUploadModel bulkReviewUploadModel = RequestHelper.toBulkReviewUploadModel(storeId,
      new StringBuilder(baseDirPath).append(file.getOriginalFilename()).toString(), actionType,
      requestId, username, null);
    log.info("Publishing event {} for bulkProcessCode = {} bulkReviewUploadModel = {} ",
      DomainEventName.BULK_REVIEW_UPLOAD_EVENT, bulkReviewUploadModel.getBulkProcessCode(),
      bulkReviewUploadModel);
    kafkaPublisher.send(DomainEventName.BULK_REVIEW_UPLOAD_EVENT,
      bulkReviewUploadModel.getBulkProcessCode(), bulkReviewUploadModel);
  }

  @Override
  public ItemSkuAndIndicativePriceResponse fetchIndicativePrice(ItemSkuListRequest request) {
    if (Objects.isNull(request) || CollectionUtils.isEmpty(request.getItemSkuList())) {
      return new ItemSkuAndIndicativePriceResponse();
    }
    GdnRestSingleResponse<ItemSkuAndIndicativePriceResponse> response =
      masterCatalogFeign.fetchIndicativePrice(request);
    ResponseHelper.validateMasterSkuResponse(response);
    return response.getValue();
  }

  private void validateUserNameAssigned(String userName) throws Exception {
    List<String> usernameList = partnersEngineService.getMasterSkuReviewers();
    if (!usernameList.contains(userName)) {
      throw new InvalidStateException(ErrorMessages.INVALID_USER_NAME);
    }
  }
}
