package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.ChangeAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.ClusterReviewFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.AcceptRejectActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.ItemSkuListRequest;
import com.gdn.partners.pcu.internal.client.model.request.MasterSkuItemsListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.CompareAnchorResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuAndIndicativePriceResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.MasterSkuConfigResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.MasterCatalogApiPath;
import com.gdn.partners.pcu.internal.service.MasterSkuReviewService;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuItemsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuInReviewDownloadWebRequest;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import java.io.IOException;
import java.util.List;

/**
 * @author Navya Naveli
 */

@Slf4j
@RestController
@RequestMapping(MasterCatalogApiPath.BASE_PATH)
public class MasterCatalogController {

  @Autowired
  private MasterSkuReviewService masterSkuReviewService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Get all master sku mapping for in-review tab")
  @PostMapping(value = MasterCatalogApiPath.IN_REVIEW_PATH,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<InReviewListResponse> getInReviewProducts(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size,
      @RequestBody InReviewListWebRequest inReviewListWebRequest) throws Exception {
    log.info("invoking controller method to get in-review products for request : {} ",
        inReviewListWebRequest);
    GdnRestListResponse<InReviewListResponse> response;
    response = masterSkuReviewService.getInReviewProductList(page, size, inReviewListWebRequest);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Get all master sku items list")
  @PostMapping(value = MasterCatalogApiPath.GET_ALL_ITEMS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ItemsListingResponse> getAllMasterSkuItems(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size,
      @RequestBody MasterSkuItemsListWebRequest masterSkuItemsListWebRequest) throws Exception {
    log.info("Getting all master sku items list with filter request : {} ", masterSkuItemsListWebRequest);
    GdnRestListResponse<ItemsListingResponse> response;
    response = masterSkuReviewService.getAllMasterSkuItemsList(page, size, masterSkuItemsListWebRequest);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Get master sku details")
  @GetMapping(value = MasterCatalogApiPath.MASTER_SKU_DETAILS, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<ItemSkuDetailResponse> getMasterSkuDetails(
    @PathVariable("itemSku") String itemSku, @RequestParam("allData") boolean allData) {
    log.info("get master sku details for item-sku = {} ", itemSku);
    GdnRestSingleResponse<ItemSkuDetailResponse> response =
      masterSkuReviewService.getMasterSkuDetails(itemSku, allData);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Fetch list of items mapped to master sku")
  @GetMapping(value = MasterCatalogApiPath.FETCH_ITEMS_MAPPED_TO_MASTER_SKU, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ItemSkuDetailResponse> fetchItemsMappedToMasterSku(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size, @PathVariable("masterSku") String masterSku) {
    log.info("Fetch list of items mapped to master sku = {} ", masterSku);
    return masterSkuReviewService.fetchItemsMappedToMasterSku(page, size, masterSku);
  }

  @Operation(summary = "Get compare anchor details")
  @GetMapping(value = MasterCatalogApiPath.COMPARE_ANCHORS, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<CompareAnchorResponse> getCompareAnchorDetails(
    @PathVariable("anchorId1") String firstAnchor, @PathVariable("anchorId2") String secondAnchor)
    throws Exception {
    log.info("Getting compare anchor details for first anchor {} and second anchor {}", firstAnchor,
      secondAnchor);
    GdnRestSingleResponse<CompareAnchorResponse> response;
    response = masterSkuReviewService.getCompareAnchorDetails(firstAnchor, secondAnchor);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Fetch item history details")
  @GetMapping(value = MasterCatalogApiPath.GET_HISTORY_DETAILS_FOR_ITEM, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ItemHistoryResponse> fetchItemHistoryDetails(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size, @PathVariable("itemSku") String itemSku) {
    log.info("Fetch history details for itemSku = {} ", itemSku);
    return masterSkuReviewService.fetchItemHistoryDetails(page, size, itemSku);
  }

  @Operation(summary = "Fetch config details for master sku review")
  @GetMapping(value = MasterCatalogApiPath.GET_MASTER_SKU_REVIEW_CONFIG, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<MasterSkuConfigResponse> fetchMasterSkuReviewConfig() {
    log.info("Fetch config details for master sku review");
    return masterSkuReviewService.fetchMasterSkuReviewConfig();
  }

  @Operation(summary = "Update accept reject action for anchor mapping")
  @PostMapping(value = MasterCatalogApiPath.ACCEPT_REJECT_ACTION,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateAcceptRejectAnchors(@PathVariable("anchorId1") String firstAnchor,
      @PathVariable("anchorId2") String secondAnchor,
      @RequestBody AcceptRejectActionRequest acceptRejectActionRequest) throws Exception {
    log.info("updating compare anchor for first anchor {}, second anchor {} and request {} ",
        firstAnchor, secondAnchor, acceptRejectActionRequest);
    acceptRejectActionRequest.setReviewedBy(clientParameterHelper.getUsername());
    masterSkuReviewService.updateAcceptRejectActionRequest(firstAnchor, secondAnchor,
        acceptRejectActionRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Perform cluster review action")
  @PutMapping(value = MasterCatalogApiPath.PERFORM_CLUSTER_REVIEW_ACTION, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse performClusterReviewAction(@PathVariable("masterSku") String masterSku,
      @RequestBody ClusterReviewFeedbackRequest request) {
    log.info("Perform action for  cluster having anchorSku = {} and request = {} ", masterSku, request);
    return masterSkuReviewService.performClusterAction(masterSku, request);
  }

  @Operation(summary = "Update assignee change for anchor mapping")
  @PostMapping(value = MasterCatalogApiPath.CHANGE_ASSIGNEE,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateChangedAssignee(@RequestBody ChangeAssigneeRequest request)
    throws Exception {
    log.info("Perform action for change assignee for request = {} ", request);
    masterSkuReviewService.updateChangeAssigneeAction(request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Bulk download in-review anchor mapping")
  @PostMapping(value = MasterCatalogApiPath.BULK_DOWNLOAD_IN_REVIEW,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkDownloadInReviewAnchors(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "0") int size,
      @RequestBody MasterSkuInReviewDownloadWebRequest request) {
    log.info("Perform bulk download for in-review anchor mappings for request = {} ", request);
    masterSkuReviewService.bulkDownloadInReviewAnchorMappings(clientParameterHelper.getUsername(),
        request, page, size);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get master sku reviewer list")
  @GetMapping(value = MasterCatalogApiPath.REVIEWERS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<List<String>> getMasterSkuReviewers() throws Exception {
    log.info("Invoking master sku reviewer list");
    List<String> response = masterSkuReviewService.getMasterSkuReviewReviewers();
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
      response);
  }

  @Operation(summary = "Download items for master sku review")
  @PostMapping(value = MasterCatalogApiPath.MASTER_SKU_ITEMS_DOWNLOAD, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadItemsForMasterSkuReview(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "0") int size,
      @RequestBody MasterSkuItemsDownloadWebRequest request) {
    log.info("Invoking items download for master sku review having request = {} ", request);
    this.masterSkuReviewService.downloadItemsForMasterSkuReview(clientParameterHelper.getUsername(),
        request, page, size);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Bulk Upload File")
  @PostMapping(value = MasterCatalogApiPath.BULK_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkUpload(
    @RequestParam("actionType") @Valid @NotBlank(message = ErrorMessages.BULK_UPLOAD_ACTION_TYPE_CANNOT_BE_NULL) String actionType,
    @RequestParam MultipartFile file) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Bulk upload file for action type {} and requestId {}", actionType, requestId);
    try {
      GdnPreconditions.checkArgument(
        BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(actionType)
          || BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(actionType),
        ErrorMessages.INVALID_ACTION_TYPE);
      masterSkuReviewService.uploadBulkFile(file, actionType, requestId,
        clientParameterHelper.getStoreId(), clientParameterHelper.getUsername());
    } catch (IOException e) {
      log.error("Error while uploading the Bulk file", e);
      return new BaseResponse(
        "Error when transferring file " + file.getOriginalFilename() + e.getMessage(), null, false,
        requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Api to fetch Indicative price")
  @PostMapping(value = MasterCatalogApiPath.FETCH_INDICATIVE_PRICE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<ItemSkuAndIndicativePriceResponse> fetchIndicativePrice(
    @RequestBody ItemSkuListRequest request) {
    return new GdnRestSingleResponse<>(masterSkuReviewService.fetchIndicativePrice(request),
      clientParameterHelper.getRequestId());
  }
}
