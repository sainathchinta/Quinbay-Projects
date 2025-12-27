package com.gdn.partners.pcu.internal.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.factory.MasterCatalogFeignFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.request.BigQueryFetchRequest;
import com.gdn.partners.pcu.internal.client.model.request.ChangeAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.ClusterReviewFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.AcceptRejectActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.ItemSkuListRequest;
import com.gdn.partners.pcu.internal.client.model.request.MasterSkuItemsListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.ClusterActionResponse;
import com.gdn.partners.pcu.internal.client.model.response.CompareAnchorResponse;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuAndIndicativePriceResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.MasterSkuConfigResponse;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author Navya Naveli
 */

@FeignClient(name = "masterCatalogFeign", url = "${service.masterCatalog.endpoint}",
    fallbackFactory = MasterCatalogFeignFallbackFactory.class)
public interface MasterCatalogFeign {

  @RequestMapping(value = "/api/products/get-in-review-items", method = RequestMethod.POST)
  GdnRestListResponse<InReviewListResponse> getItemsForInReview(
    @RequestParam(value = "page", defaultValue = "0") int page,
    @RequestParam(value = "size", defaultValue = "10") int size,
    @RequestBody InReviewListWebRequest request);

  @PostMapping(value = "/api/products/get-all-master-sku-items-list")
  GdnRestListResponse<ItemsListingResponse> getAllMasterSkuItemsList(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size, @RequestBody MasterSkuItemsListWebRequest request);

  @GetMapping(value = "/api/master-sku/{masterSku}/detail")
  GdnRestSingleResponse<ItemSkuDetailResponse> getMasterSkuDetails(
    @PathVariable("masterSku") String masterItemSku, @RequestParam("allData") boolean allData);

  @GetMapping(value = "/api/master-sku/{masterSku}/item-sku-list")
  GdnRestListResponse<ItemSkuDetailResponse> fetchItemsMappedToMasterSku(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size, @PathVariable("masterSku") String masterItemSku);

  @GetMapping(value = "/api/products/{anchorId1}/{anchorId2}/compare-anchors")
  GdnRestSingleResponse<CompareAnchorResponse> getCompareAnchorSkuDetails(
    @PathVariable("anchorId1") String firstAnchor, @PathVariable("anchorId2") String secondAnchor);

  @GetMapping(value = "/api/master-sku/{itemSku}/view-history")
  GdnRestListResponse<ItemHistoryResponse> fetchItemHistoryDetails(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size, @PathVariable("itemSku") String itemSku);

  @GetMapping(value = "/api/master-sku/config")
  GdnRestSingleResponse<MasterSkuConfigResponse> fetchMasterSkuReviewConfig();

  @PutMapping(value = "/api/master-sku/{masterSku}/clusterAction")
  GdnRestSingleResponse<ClusterActionResponse> performClusterReviewAction(
    @PathVariable("masterSku") String masterItemSku,
    @RequestBody ClusterReviewFeedbackRequest request);


  @PostMapping(value = "/api/master-sku/{anchorId1}/{anchorId2}/accept-reject-anchors")
  GdnBaseRestResponse updateAnchorsForAcceptRejectAction(
      @PathVariable("anchorId1") String firstAnchorSku, @PathVariable("anchorId2") String secondAnchorSku,
      @RequestBody AcceptRejectActionRequest request);

  @PostMapping(value = "/api/master-sku/change-assignee")
  GdnBaseRestResponse updateAssignedTo(@RequestBody ChangeAssigneeRequest request);

  @PutMapping(value = "/api/scheduled-jobs/fetchDataFromBigQuery")
  GdnBaseRestResponse fetchDataFromBigQuery(@RequestParam String processName,
      @RequestBody BigQueryFetchRequest bigQueryFetchRequest);

  @PostMapping(value = "/api/master-sku/indicative-price")
  GdnRestSingleResponse<ItemSkuAndIndicativePriceResponse> fetchIndicativePrice(
    @RequestBody ItemSkuListRequest request);
}
