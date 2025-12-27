package com.gdn.x.product.rest.web.controller.api;

import static org.springframework.data.domain.Sort.Direction.ASC;
import static org.springframework.data.domain.Sort.Direction.DESC;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.CampaignItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemsSummaryDetailRequestVo;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.rest.web.model.response.ProductSkuResponse;
import com.gdn.x.product.rest.web.model.request.ReelProductListingRequest;
import com.gdn.x.product.rest.web.model.response.ReelProductDetailResponse;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.BulkItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemSkusRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.PromoItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.CategoryService;
import com.gdn.x.product.service.api.ItemSummaryService;
import com.gdn.x.product.service.api.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping(value = ProductApiPath.SUMMARY)
@Tag(name = "Summary Controller", description = "Product And Item Summary APIs")
@Slf4j
public class SummaryController {

  private static final Logger LOG = LoggerFactory.getLogger(SummaryController.class);

  @Autowired
  private ItemSummaryService itemSummaryService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ProductService productService;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;


  @Value("${solr.max.page.size}")
  private int solrMaxPageSize;

  @RequestMapping(value = {ProductApiPath.SUMMARY_ITEM_NAME}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "false") boolean includeMarkForDelete,
      @RequestBody SimpleListStringRequest itemSkus) throws Exception {
    SummaryController.LOG
        .debug("getItemNameByItemSkus with request : {} ", new Object[] {itemSkus});
    Map<String, String> result =
        this.itemSummaryService.getItemNameByItemSkus(storeId, itemSkus.getValue(), includeMarkForDelete);
    return new GdnRestSingleResponse<SimpleMapStringResponse>(new SimpleMapStringResponse(result),
        requestId);
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.SUMMARY_FILTER}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ItemSummaryResponse> getItemSummaryByFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int page, @RequestParam int size,
      @RequestParam(required = false) String orderBy, @RequestParam(required = false) String sortBy,
      @RequestBody ItemSummaryRequest itemFilterRequest) throws Exception {
    SummaryController.LOG.debug("getItemSummaryByFilter with request : {} ", itemFilterRequest);
    ItemSummaryRequestVO requestVo = new ItemSummaryRequestVO();
    BeanUtils.copyProperties(itemFilterRequest, requestVo);
    size = size == 0 ? CommonConstants.DEFAULT_PAGE_SIZE : size;
    PageRequest pageRequest = PageRequest.of(page,size,getSort(orderBy, sortBy));
    ItemSummaryPageResponseVo result = null;
    try {
      requestVo.setPreOrderStatus(true);
      result = this.itemSummaryService
          .getItemSummaryByFilter(storeId, username, requestId, requestVo, orderBy, sortBy, pageRequest);
    } catch (ApplicationRuntimeException e) {
      SummaryController.LOG.error(
          "#getItemSummaryByFilter failed on itemSummaryRequest = {} with error = {}",
          itemFilterRequest, e.getMessage());
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    } catch (Exception e) {
      SummaryController.LOG.error(
          "#getItemSummaryByFilter failed on itemSummaryRequest = {} with error = {}",
          itemFilterRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false,
          requestId);
    }
    return this.modelConverter.convertToItemSummaryListResponse(requestId, page, size, result);
  }

  @RequestMapping(value = {ProductApiPath.NAME_FILTER}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ItemSummaryResponse> getItemNamesByFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int page, @RequestParam int size,
      @RequestParam(required = false) String orderBy, @RequestParam(required = false) String sortBy,
      @RequestBody ItemSummaryRequest itemFilterRequest) throws Exception {
    SummaryController.LOG.debug("getItemNamesByFilter with request : {} ", itemFilterRequest);
    ItemSummaryRequestVO requestVo = new ItemSummaryRequestVO();
    BeanUtils.copyProperties(itemFilterRequest, requestVo);
    size = size == 0 ? CommonConstants.DEFAULT_PAGE_SIZE : size;
    PageRequest pageRequest = PageRequest.of(page, size, getSort(orderBy, sortBy));
    ItemSummaryPageResponseVo result = null;
    try {
      result = this.itemSummaryService
          .getItemNamesByKeyword(storeId, username, requestId, requestVo, orderBy, sortBy, pageRequest);
    } catch (ApplicationRuntimeException e) {
      SummaryController.LOG
          .error("#getItemNamesByFilter failed on itemSummaryRequest = {} with error = {}", itemFilterRequest,
              e.getMessage());
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    } catch (Exception e) {
      SummaryController.LOG
          .error("#getItemNamesByFilter failed on itemSummaryRequest = {} with error = {}", itemFilterRequest,
              e.getMessage(), e);
      return new GdnRestListResponse<ItemSummaryResponse>(e.getMessage(), e.getMessage(), false, requestId);
    }
    return this.modelConverter.convertToItemSummaryListResponse(requestId, page, size, result);
  }

  @RequestMapping(value = {ProductApiPath.BULK_SUMMARY_FILTER}, method = {RequestMethod.POST},
    consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ItemSummaryResponse> getBulkItemSummaryByFilter(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam(required = false, defaultValue = "0") int page,
    @RequestParam(required = false, defaultValue = "10") int size, @RequestParam(required = false) String orderBy,
    @RequestParam(required = false) String sortBy, @RequestBody BulkItemSummaryRequest bulkItemFilterRequest) {
    LOG.debug("getBulkItemSummaryByFilter with request : {}, page {} , size {} ", bulkItemFilterRequest, page, size);

    BulkItemSummaryRequestVo requestVo = new BulkItemSummaryRequestVo();
    BeanUtils.copyProperties(bulkItemFilterRequest, requestVo);

    size = (size == 0) ? CommonConstants.DEFAULT_PAGE_SIZE : size;
    PageRequest pageRequest = PageRequest.of(page, size, getSort(orderBy, sortBy));
    try {
      ItemSummaryPageResponseVo result = this.itemSummaryService
        .getBulkItemSummaryByFilter(storeId, username, requestId, requestVo, pageRequest, sortBy, orderBy);
      LOG.debug("item summary response {}", result);

      return this.modelConverter.convertToItemSummaryListResponse(requestId, page, size, result);
    } catch (SolrCustomException e) {
      SummaryController.LOG.error(
        "#getBulkItemSummaryByFilter failed on bulkItemSummaryRequest = {} with error = ",
        bulkItemFilterRequest, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
    catch(Exception e) {
      LOG.error("#getBulkItemSummaryByFilter failed on bulkItemSummaryRequest = {}", bulkItemFilterRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    }
  }

  private Sort getSort(String orderBy, String sortBy) {
    return StringUtils.isNotEmpty(orderBy) ?
      (Sort.by("asc".equalsIgnoreCase(sortBy) ? ASC : DESC, orderBy)) :
      Sort.unsorted();
  }

  @RequestMapping(value = {ProductApiPath.SUMMARY_ARCHIVED_FILTER}, method = {RequestMethod.POST},
                  consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ItemSummaryResponse> getArchivedItemSummaryByFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int page, @RequestParam int size,
      @RequestBody ItemSummaryRequest itemFilterRequest) throws Exception {
    SummaryController.LOG.debug("#getArchivedItemSummaryByFilter with request : {} ",
        new Object[] {itemFilterRequest});
    ItemSummaryRequestVO requestVo = new ItemSummaryRequestVO();
    BeanUtils.copyProperties(itemFilterRequest, requestVo);
    ItemSummaryPageResponseVo result = null;
    try {
      result =
          this.itemSummaryService.getItemSummaryByArchivedFilter(storeId, username, requestId, requestVo,
              PageRequest.of(page,size));
    } catch (ApplicationRuntimeException e) {
      SummaryController.LOG.error(
          "#getArchivedItemSummaryByFilter failed on itemSummaryRequest = {} with error = {}",
          itemFilterRequest, e.getMessage());
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    } catch (Exception e) {
      SummaryController.LOG.error(
          "#getArchivedItemSummaryByFilter failed on itemSummaryRequest = {} with error = {}",
          itemFilterRequest, e.getMessage(), e);
      return new GdnRestListResponse<ItemSummaryResponse>(e.getMessage(), e.getMessage(), false,
          requestId);
    }
    return this.modelConverter.convertToItemSummaryListResponse(requestId, page, size, result);
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.BY_MASTER_CATALOG}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(deprecated = true)
  public GdnRestListResponse<ProductSummaryResponse> getListOfProductSummaryByMasterCatalog(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String catalogCode, @RequestParam String categoryCode,
      @RequestParam boolean searchEmptySalesOnly, @RequestParam int page, @RequestParam int size)
      throws Exception {
    SummaryController.LOG
        .info(
            "getListOfProductSummaryByMasterCatalog, with request : catalogCode {}, catalogCode {}, searchEmptySalesOnly {}",
            catalogCode, categoryCode, searchEmptySalesOnly);
    Page<ProductAndItemSolr> result = null;
    try {
      result = this.categoryService.getProductsByMasterCatalog(storeId, catalogCode, categoryCode,
        searchEmptySalesOnly, PageRequest.of(page, size));
    } catch (SolrCustomException e) {
      SummaryController.LOG.error(
        "#getListOfProductSummaryByMasterCatalog failed for category code = {} with error = ",
        categoryCode, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    } catch (Exception e) {
      SummaryController.LOG.error(
        "#getListOfProductSummaryByMasterCatalog failed for category code = {} with error = ",
        categoryCode, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    }
    return this.modelConverter.convertToProductSummaryResponse(result, requestId, page, size);
  }


  @RequestMapping(value = {ProductApiPath.BY_SALES_CATALOG}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ProductSummaryResponse> getListOfProductSummaryBySalesCatalog(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String catalogCode, @RequestParam String categoryCode, @RequestParam int page,
      @RequestParam int size) throws Exception {
    SummaryController.LOG.info(
        "getListOfProductSummaryBySalesCatalog, with request : catalogCode {}, catalogCode {}",
        catalogCode, categoryCode);
    try {
      Page<ProductAndItemSolr> result =
        this.categoryService.getProductsBySalesCatalog(storeId, catalogCode, categoryCode,
          PageRequest.of(page, size));
      return this.modelConverter.convertToProductSummaryResponse(result, requestId, page, size);
    }
    catch (SolrCustomException e){
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(),false,requestId);
    }
    catch (Exception e){
      return new GdnRestListResponse<>(e.getMessage(),e.getMessage(),false,requestId);
    }
  }


  @RequestMapping(value = {ProductApiPath.SUMMARY_PRODUCT_NAME}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<SimpleMapStringResponse> getProductNameByProductSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest productSkus) throws Exception {
    SummaryController.LOG.debug("getProductNameByProductSkus with request : {} ",
        new Object[] {productSkus});
    Map<String, String> result =
        this.itemSummaryService.getProductNameByProductSkus(storeId, productSkus.getValue());
    return new GdnRestSingleResponse<SimpleMapStringResponse>(new SimpleMapStringResponse(result),
        requestId);
  }

  @RequestMapping(value = {ProductApiPath.GET_PRODUCT_NAME_BY_CODE}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<SimpleMapStringResponse> getProductNamesByProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest productCodes) throws Exception {
    SummaryController.LOG.debug("getProductNamesByProductCodes with request : {} ",
        new Object[] {productCodes});
    Map<String, String> result =
        this.itemSummaryService.getProductNamesByProductCodes(storeId, productCodes.getValue());
    return new GdnRestSingleResponse<SimpleMapStringResponse>(new SimpleMapStringResponse(result),
        requestId);
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.SUMMARY_SINGLE}, method = {RequestMethod.GET}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<ItemSummaryResponse> getSingleItemSummaryByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam(required = false) String fetchViewConfigByChannel) throws Exception {
    SummaryController.LOG.debug("getSingleItemSummaryByItemSku with request : {} ", new Object[] {itemSku});
    try {
      ItemSummaryPageResponseVo result =
          this.itemSummaryService.getItemSummaryByItemSku(storeId, username, requestId, itemSku,
              fetchViewConfigByChannel);
      return this.modelConverter.convertToItemSummarySingleResponse(requestId, result);
    } catch (Exception e) {
      SummaryController.LOG.error("error on fetching the summary for item sku : {}", itemSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.SUMMARY_SINGLE_BY_ITEM_SKU.getCode(),
          false, null, requestId);
    }
  }


  @Deprecated
  @RequestMapping(value = {ProductApiPath.ARCHIVED_SUMMARY_SINGLE}, method = {RequestMethod.GET},
                  produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<ItemSummaryResponse> getSingleArchivedItemSummaryByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku) throws Exception {
    SummaryController.LOG.debug("getSingleArchivedItemSummaryByItemSku with request : {} ", itemSku);
    ItemSummaryRequestVO requestVo = new ItemSummaryRequestVO();
    requestVo.setItemSkus(Collections.singletonList(itemSku));
    ItemSummaryPageResponseVo result =
        this.itemSummaryService.getItemSummaryByArchivedFilter(storeId, username, requestId, requestVo,
            PageRequest.of(0,1));
    return this.modelConverter.convertToItemSummarySingleResponse(requestId, result);
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.SUMMARY_UPDATE}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<ItemSummaryResponse> updateItemSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku, @RequestParam String merchantCode,
      @RequestBody UpdateItemSummaryRequest updateItemSummaryRequest) throws Exception {
    SummaryController.LOG.info("updateItemSummary with request : {} - itemSku:{} merchantCode:{}",
        updateItemSummaryRequest, itemSku, merchantCode);
    UpdateItemSummaryRequestVo requestVo =
        this.modelConverter.convertToUpdateItemSummaryVo(updateItemSummaryRequest);
    ItemSummaryResponse itemSummaryResponse = null;
    try {
      itemSummaryResponse = this.modelConverter.convertToResponse(this.itemSummaryService
          .updateItemSummary(storeId, requestId, username, itemSku, merchantCode, requestVo,
              updateItemSummaryRequest.getWholesalePriceActivated()), ItemSummaryResponse.class);
      return new GdnRestSingleResponse<>(itemSummaryResponse, requestId);
    } catch (Exception e) {
      SummaryController.LOG.error("error on update item summary {} {} {}", itemSku, merchantCode,
          new Object[] {updateItemSummaryRequest}, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM.getCode(), false,
          itemSummaryResponse, requestId);
    }
  }

  @RequestMapping(value = {
      ProductApiPath.LISTING_UPDATE}, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateItemListing(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productSku") String productSku, @RequestBody ItemListingUpdateRequest itemListingUpdateRequest)
      throws Exception {
    SummaryController.LOG.info("updateItemListing with request : productSku:{} {} ", productSku,
        new Object[] {itemListingUpdateRequest});
    List<ItemListingUpdateRequestVo> requestVo =
        this.modelConverter.convertToItemListingUpdateRequestVo(itemListingUpdateRequest);
    try {
      this.itemSummaryService
          .updateItemListing(storeId, requestId, username, productSku, itemListingUpdateRequest.getProductType(),
              requestVo);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      SummaryController.LOG
          .error("error on update item listing {} {} {}", productSku, new Object[] {itemListingUpdateRequest}, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.SUMMARY_BY_CATEGORY_AND_BRAND}, method = {RequestMethod.POST},
                  consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ItemSummaryResponse> getItemSummaryByCategoryAndBrandFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size,
      @RequestBody CampaignItemSummaryRequest campaignItemSummaryRequest) {
    SummaryController.LOG
        .debug("getItemSummaryByCategoryAndBrandFilter with requestId - {} & request -  {} ",
            requestId, campaignItemSummaryRequest);
    CampaignItemSummaryRequestVO requestVo =
        new CampaignItemSummaryRequestVO(campaignItemSummaryRequest.getMerchantCode(),
            campaignItemSummaryRequest.getCategories(), campaignItemSummaryRequest.getBrands(),
            campaignItemSummaryRequest.getKeyword(), campaignItemSummaryRequest.getItemSku());
    PageRequest pageRequest = PageRequest.of(page, size);
    ItemSummaryPageResponseVo result = null;
    try {
      result = this.itemSummaryService
          .getCampaignItemSummaryByFilter(storeId, username, requestId, requestVo, pageRequest);
    } catch (ApplicationRuntimeException e) {
      SummaryController.LOG.error(
          "#getItemSummaryByCategoryandBrandFilter failed on campaignItemSummaryRequest = {} with"
              + " error = {}",
          campaignItemSummaryRequest, e.getMessage());
      return new GdnRestListResponse<ItemSummaryResponse>(e.getErrorMessage(),
          e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      SummaryController.LOG.error(
          "#getItemSummaryByCategoryandBrandFilter failed on campaignItemSummaryRequest = {} with"
              + " error = {}",
          campaignItemSummaryRequest, e.getMessage(), e);
      return new GdnRestListResponse<ItemSummaryResponse>(e.getMessage(), null, false,
          requestId);
    }
    return this.modelConverter.convertToItemSummaryListResponse(requestId, page, size, result);
  }

  @RequestMapping(value = {ProductApiPath.PROMO_ITEM_SUMMARY_FILTER}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnRestListResponse<ItemSummaryResponse> getPromoItemSummaryByFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam int page, @RequestParam int size,
      @RequestParam(required = false) String orderBy, @RequestParam(required = false) String sortBy,
      @RequestBody PromoItemSummaryRequest promoItemSummaryRequest) throws Exception {
    SummaryController.LOG.debug("getPromoItemSummaryByFilter with request : {} ", promoItemSummaryRequest);
    ItemSummaryRequestVO requestVo = new ItemSummaryRequestVO();
    BeanUtils.copyProperties(promoItemSummaryRequest, requestVo);
    PageRequest pageRequest =
        PageRequest.of(page, size > solrMaxPageSize ? solrMaxPageSize : size, getSort(orderBy,
      sortBy));
    try {
      ItemSummaryPageResponseVo result = this.itemSummaryService
          .getPromoItemSummaryByFilter(storeId, requestId, username, requestVo, orderBy, sortBy, pageRequest);
      GdnRestListResponse<ItemSummaryResponse> response =
        this.modelConverter.convertToItemSummaryListResponse(requestId, page,
          size > solrMaxPageSize ? solrMaxPageSize : size, result);
      return new GdnRestListResponse<>(response.getContent(),response.getPageMetaData(),requestId);
    } catch (SolrCustomException e) {
      SummaryController.LOG.error(
        "#getPromoItemSummaryByFilter failed on request = {} with error = {}",
        promoItemSummaryRequest, e.getMessage());
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    } catch (ApplicationRuntimeException e) {
      SummaryController.LOG.error(
        "#getPromoItemSummaryByFilter failed on request = {} with error = {}",
        promoItemSummaryRequest, e.getMessage());
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false,
        requestId);
    } catch (Exception e) {
      SummaryController.LOG.error(
          "#getPromoItemSummaryByFilter failed on request = {} with error = {}",
          promoItemSummaryRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_CENTER_SUMMARY_FILTER, method = RequestMethod.POST, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ProductCenterSummaryResponse> getProductCenterSummaryFilter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam int page, @RequestParam(defaultValue = "100") int size,
      @RequestBody ProductCenterSummaryRequest productCenterSummaryRequest) throws Exception {
    SummaryController.LOG.debug("get with request productCenterSummaryRequest: {} ", productCenterSummaryRequest);
    PageRequest pageRequest = PageRequest.of(page, size);
    try {
      Page<ProductCenterSummaryResponse> result =
          productService.getProductCenterSummary(storeId, requestId, productCenterSummaryRequest, pageRequest);
      return new GdnRestListResponse<ProductCenterSummaryResponse>(result.getContent(),
          new PageMetaData(size, page, result.getTotalElements()), requestId);
    } catch (Exception e) {
      SummaryController.LOG
          .error("#getProductCenterSummaryByFilter failed on request = {} with error = {}", productCenterSummaryRequest,
              e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.SUMMARY_PRODUCT_SKU_LIST, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ProductSkuSummaryResponse> getProductSkuSummaryList(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "25") Integer size,
      @RequestBody ProductSkuSummaryRequest productSkuSummaryRequest) throws Exception {
    SummaryController.LOG.debug("Get product sku list with productSkuSummaryRequest : {} ",
        productSkuSummaryRequest);
    try {
      Page<ProductSkuSummaryResponse> productSkuSummaryResponses =
          this.productService.getProductSkuList(storeId, productSkuSummaryRequest, businessPartnerCode, page, size);
      return new GdnRestListResponse<>(null, null, true, productSkuSummaryResponses.getContent(),
          new PageMetaData(size, page, productSkuSummaryResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      SummaryController.LOG.error("Error fetching product sku list with request : {}, error : {} ",
          productSkuSummaryRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.DETAIL_SUMMARY_FILTER}, method = {RequestMethod.POST}, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ItemSummaryDetailResponse> getItemsSummaryDetailByFilter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam int page,
      @RequestParam(defaultValue = "10") int size,
      @RequestParam(required = false) String fetchViewConfigByChannel,
      @RequestBody ItemsSummaryDetailRequest itemsSummaryDetailRequest) throws Exception {
    SummaryController.LOG.debug("getItemsSummaryDetailByFilter with request : {} ", itemsSummaryDetailRequest);
    ItemsSummaryDetailRequestVo requestVo = new ItemsSummaryDetailRequestVo();
    BeanUtils.copyProperties(itemsSummaryDetailRequest, requestVo);
    ItemSummaryPageResponseVo result;
    try {
      result = this.itemSummaryService.
          getItemsSummaryDetailByFilter(storeId, username, requestId,
          requestVo, page, size);
    } catch (ApplicationRuntimeException e) {
      LOG.error("#getItemSummaryByFilter failed on getItemsSummaryDetailByFilter = {} with error = {}",
          itemsSummaryDetailRequest, e.getMessage());
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    } catch (Exception e) {
      LOG.error("#getItemSummaryByFilter failed on getItemsSummaryDetailByFilter = {} with error = {}",
          itemsSummaryDetailRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    }
    return this.modelConverter.convertToItemSummaryDetailListResponse(requestId, page, size, result,
        fetchViewConfigByChannel);
  }

  @RequestMapping(value = {ProductApiPath.GET_L3_PRODUCT_LIST}, method = {RequestMethod.POST}, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get L3 Summary with keyword/merchantCode")
  public GdnRestListResponse<ProductL3SummaryResponse> getFilterSummaryL3(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam int page,
      @RequestParam(defaultValue = "10") int size,
      @RequestParam(required = false, defaultValue = "true") boolean onlyDefaultViewConfig,
      @RequestBody ProductSummaryRequest productSummaryRequest)
      throws Exception {
    SummaryController.LOG.debug("ProductSummaryRequest with request : {} ", productSummaryRequest);
    PageRequest pageRequest = PageRequest.of(page,size);
    Page<ProductL3SummaryResponse> result;
    try {
      result = this.productService
          .getProductL3SummaryResponse(storeId, productSummaryRequest, pageRequest, onlyDefaultViewConfig);
    } catch (ApplicationRuntimeException e) {
      LOG.error("#getProductSummaryRequest failed on getFilterSummaryL3 = {} with error = {}", productSummaryRequest,
          e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId);
    } catch (Exception e) {
      LOG.error("#getProductSummaryRequest failed on getFilterSummaryL3 = {} with error = {}",
          productSummaryRequest, e.getMessage(), e);
      return new GdnRestListResponse(e.getMessage(), e.getMessage(), false, requestId);
    }
    return new GdnRestListResponse(result.getContent(),
        new PageMetaData(size, page, result.getTotalElements()), requestId);
  }

  @PostMapping(value = ProductApiPath.GET_PRODUCT_SKU_LIST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get only product sku list with merchantCode/categoryCode/pickupPointCode/promoType")
  public GdnRestListResponse<ProductSkuResponse> getFilterProductSkuList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size, @RequestBody ProductSummaryRequest productSummaryRequest)
      throws Exception {
    SummaryController.LOG.debug("ProductSummaryRequest with request : {} ", productSummaryRequest);
    PageRequest pageRequest = PageRequest.of(page,size);
    Page<ProductSkuResponse> result;
    try {
      result = this.productService
          .getProductSkuListResponse(storeId, productSummaryRequest, pageRequest);
    } catch (ApplicationRuntimeException e) {
      LOG.error("#getProductSkuListResponse failed on getFilterProductSkuList = {} with error = {}", productSummaryRequest,
          e.getMessage(), e);
      return new GdnRestListResponse<ProductSkuResponse>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId);
    } catch (Exception e) {
      LOG.error("#getProductSummaryRequest failed on getFilterProductSkuList = {} with error = {}",
          productSummaryRequest, e.getMessage(), e);
      return new GdnRestListResponse(e.getMessage(), e.getMessage(), false, requestId);
    }
    return new GdnRestListResponse(result.getContent(),
        new PageMetaData(size, page, result.getTotalElements()), requestId);
  }

  @RequestMapping(value = {ProductApiPath.PRODUCT_NAME_FILTER}, method = {RequestMethod.POST}, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<ProductNameSuggestionResponse> getProductNamesByFilter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "50") int size, @RequestParam(required = false) String orderBy,
      @RequestParam(required = false) String sortBy, @RequestBody ProductSummaryRequest productSummaryRequest)
      throws Exception {
    LOG.info("getProductNamesByFilter with request : {} ", productSummaryRequest);
    PageRequest pageRequest = PageRequest.of(page, size, getSort(orderBy, sortBy));
    Page<ProductNameSuggestionResponse> result;
    try {
      result = this.productService.getProductNamesByKeyword(storeId, productSummaryRequest, pageRequest);
    } catch (Exception e) {
      LOG.error("getProductNamesByFilter failed on request = {} error = {}", productSummaryRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    }
    return new GdnRestListResponse<>(result.getContent(), new PageMetaData(size, page, result.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = {ProductApiPath.GET_ITEM_SUMMARY_BY_ITEM_LIST}, method = {RequestMethod.POST}, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnRestListResponse<ItemSummaryListResponse> getItemSummaryByItemSkusList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ItemSkusRequest itemSkusRequest) throws Exception {
    LOG.info("getItemSummaryByItemSkusList with request : {} ", itemSkusRequest);
    List<ItemSummaryListResponse> result;
    try {
      result = this.itemSummaryService.getItemSummaryByItemSkusList(storeId, itemSkusRequest.getItemSkus());
    }
    catch (ApplicationRuntimeException e) {
      SummaryController.LOG.error(
        "#getItemSummaryByItemSkusList failed on itemSku Request : {} with error : {} ",
        itemSkusRequest, e.getMessage());
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
    catch (Exception e) {
      LOG.error("getItemSummaryByItemSkusList failed on request = {} error = {}", itemSkusRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, requestId);
    }
    return new GdnRestListResponse<>(result, new PageMetaData(result.size(), 0, result.size()), requestId);
  }


  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPDATE_WHOLESALE_ACTIVATION_FLAG}, method = {RequestMethod.POST}, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse updateWholeSaleActivationFlag(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam boolean wholeSalePriceActivated, @RequestBody SimpleListStringRequest itemSkus) throws Exception {
    LOG.info("updateWholeSaleActivationFlag with request : {} ", itemSkus);
    List<ItemSummaryListResponse> result;
    try {
      this.itemSummaryService.updateWholeSaleActivationFlag(storeId, itemSkus.getValue(), wholeSalePriceActivated);
    } catch (Exception e) {
      LOG.error("updateWholeSaleActivationFlag failed on request = {} error = {}", itemSkus, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), e.getMessage(), false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.PICKUP_POINT_DETAIL_BY_CODES, method = RequestMethod.POST,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "Fetch pickup point details for list of PP codes")
  public GdnRestListResponse<PickupPointDetailResponse> getPickupPointDetailByCodes(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody SimpleListStringRequest pickupPointCodes) throws Exception {
    log.info("Fetch pickup point details for request : {} ", pickupPointCodes.getValue());
    try {
      List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
        this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
          storeId, pickupPointCodes.getValue());
      return new GdnRestListResponse<>(null, null, true,
          this.modelConverter.convertToPickupPointDetailResponseList(businessPartnerPickupPoints,
              pickupPointCodes.isFbbActivated()), null, requestId);
    } catch (Exception e) {
      log.error("Fetch pickup point details failed on request = {} error = {}", pickupPointCodes,
        e.getMessage(), e);
      return new GdnRestListResponse(e.getMessage(), e.getMessage(), false, null, null, requestId);
    }
  }

  @PostMapping(value = ProductApiPath.PRODUCT_LIST_FOR_REELS, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "Fetch all TD products to map to reels", summary = "Fetch all TD "
      + "products to map to reels")
  public GdnRestListResponse<ReelProductDetailResponse> getProductsToMapToReels(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size,
      @RequestBody ReelProductListingRequest reelProductListingRequest) {
    log.info("Fetching products to map to reels with request {} ", reelProductListingRequest);
    PageRequest pageRequest = PageRequest.of(page, size);
    try {
      Page<ReelProductDetailResponse> reelProductDetailResponses;
      reelProductDetailResponses =
          this.productService.getProductDetailsByReelProductListingRequest(storeId,
              reelProductListingRequest, pageRequest);
      return new GdnRestListResponse<>(null, null, true, reelProductDetailResponses.getContent(),
          new PageMetaData(size, page, reelProductDetailResponses.getTotalElements()), requestId);
    } catch (Exception exception) {
      log.error("Error while fetching products on request = {} error = {}",
          reelProductListingRequest, exception.getMessage(), exception);
      return new GdnRestListResponse(exception.getMessage(), exception.getMessage(), false, null,
          null, requestId);
    }
  }
}
