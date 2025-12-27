package com.gdn.x.product.rest.web.controller.api;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.rest.web.model.request.UpcStatusRequest;
import com.gdn.x.product.rest.web.model.response.ItemBasicL4Response;
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.ItemAndBundlingInfoVO;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.rest.web.model.AgpConstant;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.DefaultPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemAndBundlingInfoRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductTypeEditRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateDGLevelRequest;
import com.gdn.x.product.rest.web.model.response.DefaultItemSkuResponse;
import com.gdn.x.product.rest.web.model.response.ItemAndBundlingInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.PristineCategoryAttributeMapResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemSummaryService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.executor.api.AsyncProcessor;
import com.gdn.x.product.service.util.ModelConverter;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping(value = ProductApiPath.ITEM)
@Tag(name = "Item",
    description = "Item Service API")
@Slf4j
public class ItemController {

  private static final String NO_ITEM_UPDATED_WITH_SKU = "no item updated with sku ";

  private static final Logger LOG = LoggerFactory.getLogger(ItemController.class);
  private int DEFAULT_PAGE_SIZE = 10;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemSummaryService itemSummaryService;

  @Autowired
  private ItemViewConfigService itemViewConfigService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private AsyncProcessor asyncProcessor;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Deprecated
  @RequestMapping(value = {ProductApiPath.ADD}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "add item", description = "add item to prd_item collection")
  public GdnBaseRestResponse addItem(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String productSku, @RequestBody ItemDTO itemDTO) {

    ItemController.LOG.info("Add new item with storeId = {}, and itemRequest = {}", new Object[] {storeId, itemDTO});

    try {
      this.itemService.addItem(storeId, requestId, username, productSku, this.modelConverter.convertItemDTOToItem(itemDTO));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ItemController.LOG.error("#addItem request = {}, failed with error = {}", itemDTO, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_ITEM.getCode(), false, requestId);
    } catch (Exception e) {
      ItemController.LOG.error("#addItem request = {}, failed with error = {}", itemDTO, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_ITEM.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.ADD_PRICE}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "add item price", description = "add item price to prd_item collection")
  public GdnBaseRestResponse addItemPrice(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku, @RequestBody PriceRequest priceRequest) {

    ItemController.LOG.info("Add item price with storeId = {}, itemSku = {}, priceRequest = {}",
      storeId, itemSku, priceRequest);

    try {
      this.itemService.addItemPrice(storeId, this.modelConverter.convertToItemPrice(priceRequest),
        itemSku, username);

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);

      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_ITEM_PRICE.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE}, method = {
    RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "add item price", description = "add item price by merchant sku and merchant code")
  public GdnBaseRestResponse addItemPriceByMerchantSkuAndMerchantCode(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String merchantSku,
    @RequestParam String merchantCode, @RequestBody PriceRequest priceRequest) {

    ItemController.LOG.info(
      "Add item price with storeId = {}, merchantSku = {}, merchantCode = {}, priceRequest = {}",
      new Object[] {storeId, merchantSku, merchantCode, priceRequest});

    try {
      this.itemService.addItemPriceByMerchantSkuAndMerchantCode(storeId, requestId, username,
        merchantSku, merchantCode, this.modelConverter.convertToItemPrice(priceRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE.getCode(), false,
        requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.ADD_VIEW_CONFIG, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add a new item view config", description =
    "add a new item view config for specified channel, including discoverable and " + "buyable schedule for that channel")
  public GdnBaseRestResponse addItemViewConfig(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku, @RequestBody ItemViewConfigRequest itemViewConfigRequest) {
    ItemController.LOG.info("Add itemViewConfig with itemSku = {}, itemViewConfigRequest = {}", new Object[] {itemSku, itemViewConfigRequest});

    try {
      this.itemViewConfigService.addItemViewConfig(storeId, itemSku, this.modelConverter.convertToItemViewConfig(itemViewConfigRequest));

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE}, method = {
    RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "add a new item view config", description = "add a new item view config by merchant and merchant code")
  public GdnBaseRestResponse addItemViewConfigByMerchantSkuAndMerchantCode(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String merchantSku, @RequestParam String merchantCode,
    @RequestBody ItemViewConfigRequest itemViewConfigRequest) {

    ItemController.LOG.info(
      "Add a new item view config with storeId = {}, merchantSku = {}, merchantCode = {}, " + "itemViewConfigRequest = {}",
      new Object[] {storeId, merchantSku, merchantCode, itemViewConfigRequest});

    try {
      this.itemViewConfigService.addItemViewConfigByMerchantSkuAndMerchantCode(storeId, requestId,
        username, merchantSku, merchantCode, this.modelConverter.convertToItemViewConfig(itemViewConfigRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ADD_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.DELETE}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete item", description = "delete item by setting mark for delete true for item")
  public GdnBaseRestResponse deleteItem(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku) {

    ItemController.LOG.info("Delete item with storeId = {}, and itemSku = {}", new Object[] {storeId, itemSku});

    try {
      this.itemService.deleteItem(storeId, itemSku);

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.DELETE_ITEM.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.ARCHIVE}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "archive item", description = "archiving item")
  public GdnBaseRestResponse toogleArchiveItem(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku,
    @RequestParam Boolean doArchive) {

    ItemController.LOG.info("Archive item with storeId = {}, and itemSku = {}",
      new Object[] {storeId, itemSku});

    try {
      EditItemResponse response =
        this.itemService.toggleArchiveItem(storeId, itemSku, username, doArchive);
      if (Objects.nonNull(response.getApiErrorCode())) {
        LOG.error("Error while updating the edit archive for itemSku : {} and error code : {}",
          itemSku, response);
        return new GdnBaseRestResponse(response.getApiErrorCode().getDesc(),
          response.getApiErrorCode().getCode(), false, requestId);
      } else {
        return new GdnBaseRestResponse(null, null, true, requestId);
      }
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.ARCHIVE_ITEM.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.DELETE_PRICE}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete item price", description = "delete item price in prd_item collection")
  public GdnBaseRestResponse deleteItemPriceByChannel(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam String itemSku, @RequestParam String channel) {

    ItemController.LOG.info("Delete item price with storeId = {}, itemSku = {}, channel = {}",
      storeId, itemSku, channel);

    try {
      this.itemService.deleteItemPrice(storeId, itemSku, channel);

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.DELETE_ITEM_PRICE.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.DELETE_VIEW_CONFIG, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete an item view config", description =
    "delete an item view config for specified channel, including buyable and " + "discoverable schedule for that channel")
  public GdnBaseRestResponse deleteItemViewConfig(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku, @RequestParam String channel) {
    ItemController.LOG.info("Delete itemViewConfig with itemSku = {}, channel = {}", new Object[] {itemSku, channel});

    try {
      this.itemViewConfigService.deleteItemViewConfig(storeId, itemSku, channel);

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);

      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.DELETE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.GET}, method = {RequestMethod.GET}, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get item", description = "get item from prd_item collection")
  public GdnRestSingleResponse<ItemResponse> getItem(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku) {

    ItemController.LOG.info("Get item with storeId = {}, and itemSku = {}", new Object[] {storeId, itemSku});

    try {
      Item item = this.itemService.getItem(storeId, requestId, username, itemSku, true);

      return new GdnRestSingleResponse<ItemResponse>(null, null, true, this.modelConverter.convertToItemResponse(item), requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnRestSingleResponse<ItemResponse>(e.getMessage(), ProductErrorCodesEnum.GET_ITEM.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.IS_PICKUP_POINT_CODE_USED, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "check if pickup point code is used on item", description = "check if pickup "
    + "point code is used on item")
  public GdnRestSingleResponse<SimpleBooleanResponse> isPickupPointCodeUsed(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String pickupPointCode) {
    ItemController.LOG.info("isPickupPointCodeUsed with pickupPointCode = {}", new Object[] {pickupPointCode});
    SimpleBooleanResponse response = new SimpleBooleanResponse(this.productSearchService.isPickupPointCodeUsed(storeId,
        pickupPointCode));
    return new GdnRestSingleResponse<SimpleBooleanResponse>(response, requestId);
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.UPDATE_ETD_NOTE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update etd note", description = "update etd note, only read item sku and etd "
    + "note inside the request")
  @ResponseBody
  public GdnBaseRestResponse updateEtdNote(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku, @RequestBody SimpleRequestHolder request) {
    ItemController.LOG.info("Update itemViewConfig with itemSku = {}, itemViewConfigRequest = {}",
      new Object[] {itemSku, request});
    try {
      boolean successUpdate = this.itemService.updateEtdNote(storeId, itemSku, request.getId());
      if (successUpdate) {
        return new GdnBaseRestResponse(null, null, true, requestId);
      } else {
        return new GdnBaseRestResponse(ItemController.NO_ITEM_UPDATED_WITH_SKU + itemSku,
          ProductErrorCodesEnum.NO_ITEM_UPDATED.getCode(), false, requestId);
      }
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.PUBLISH_ALL_ITEMS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish all items")
  @ResponseBody
  public GdnBaseRestResponse publishAllItems(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username) {
    asyncProcessor.submitWithBackoff(AgpConstant.COMMAND_PUBLISH_ALL_ITEMS, () -> this.itemService.publishAllItems(storeId));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.REPUBLISH_ITEMS_TO_AGP, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "republish items to AGP")
  @ResponseBody
  public GdnBaseRestResponse republishItemsToAgp(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestBody List<String> itemSkus) {
    asyncProcessor.submitWithBackoff(AgpConstant.COMMAND_REPUBLISH_ITEMS_TO_AGP,
      () -> this.itemService.republishItemsToAgp(storeId, itemSkus));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = {ProductApiPath.UPDATE}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item", description = "update item only on specified field")
  public GdnRestSingleResponse<ItemResponse> updateItem(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(required = false, defaultValue = "true") boolean isOnlyExternal, @RequestParam(required = false, defaultValue = "false") boolean isProductTypeChanged,
    @RequestParam(required = false, defaultValue = "false") boolean isPreOrderChanged, @RequestBody ItemRequest itemRequest) {

    ItemController.LOG.info("Update item with storeId = {}, and itemRequest = {}", new Object[] {storeId, itemRequest});
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);

    try {
      Item item =
        this.itemService.updateItem(storeId, this.modelConverter.convertToItem(itemRequest),
          username, isOnlyExternal, isProductTypeChanged, isPreOrderChanged);
      return new GdnRestSingleResponse<>(null, null, true, this.modelConverter.convertToItemResponse(item), requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM.getCode(), false, null,
        requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.UPDATE_DG_LEVEL_FOR_OLD_PRODUCT}, method = {
    RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item view configuration", description = "update item view/visibility "
    + "configuration by merchantSku and merchantCode")
  public GdnBaseRestResponse updateItemDangerousGoodsForOldProduct(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestBody List<UpdateDGLevelRequest> updateDGLevelRequest) {

    ItemController.LOG.info(
      "updateItemDangerousGoodsForOldProduct with storeId = {}, UpdateDGLevelRequest = {}", storeId,
      updateDGLevelRequest);

    try {
      Map<Integer, Set<String>> dangerousGoodsMap = updateDGLevelRequest.stream().collect(
        Collectors.groupingBy(UpdateDGLevelRequest::getDangerousLevel, Collectors.mapping(UpdateDGLevelRequest::getItemSku, Collectors.toSet())));
      itemService.updateDangerousGoodsLevel(storeId, dangerousGoodsMap);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_DG_LEVEL.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPDATE_PRICE}, method = {RequestMethod.POST}, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item price", description = "update item price to prd_item collection")
  public GdnBaseRestResponse updateItemPrice(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam String itemSku, @RequestBody PriceRequest priceRequest) {

    ItemController.LOG.info("Update item price with storeId = {}, itemSku = {}, priceRequest = {}",
      storeId, itemSku, priceRequest);

    try {
      this.itemService.updateItemPrice(username, storeId, this.modelConverter.convertToItemPrice(priceRequest),
        itemSku, priceRequest.getWholesalePriceActivated());

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_PRICE.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE}, method = {
    RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item price", description = "update item price by merchantSku and "
    + "merchantCode")
  public GdnBaseRestResponse updateItemPriceByMerchantSkuAndMerchantCode(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username, @RequestParam String merchantSku,
    @RequestParam String merchantCode, @RequestBody PriceRequest priceRequest) {

    ItemController.LOG.info(
      "Update item price with storeId = {}, merchantSku = {}, merchantCode = {}, priceRequest =" + " {}", storeId, merchantSku, merchantCode, priceRequest);

    try {
      this.itemService.updateItemPriceByMerchantSkuAndMerchantCode(storeId, requestId, username,
        merchantSku, merchantCode, this.modelConverter.convertToItemPrice(priceRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.UPDATE_VIEW_CONFIG, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update item view config", description =
    "update item view config for specified channel, including discoverable and buyable " + "schedule for that channel")
  public GdnBaseRestResponse updateItemViewConfig(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String itemSku, @RequestBody ItemViewConfigRequest itemViewConfigRequest) {
    ItemController.LOG.info("Update itemViewConfig with itemSku = {}, itemViewConfigRequest = {}",
      new Object[] {itemSku, itemViewConfigRequest});

    try {
      this.itemViewConfigService.updateItemViewConfig(storeId, itemSku, this.modelConverter.convertToItemViewConfig(itemViewConfigRequest));

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);

      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.UPDATE_VIEW_CONFIG_WITH_ITEM_STATUS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update item view config", description = "update item view config by checking "
    + "the status of item")
  @ResponseBody
  public GdnBaseRestResponse updateItemViewConfigWithItemStatus(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @PathVariable("itemSku") String itemSku, @RequestBody ItemViewConfigBaseRequest itemViewConfigBaseRequest) {
    LOG.info("Update itemViewConfig with itemSku = {}, itemViewConfigRequest = {}", itemSku,
      itemViewConfigBaseRequest);
    try {
      this.itemViewConfigService.updateProductItemViewConfig(storeId, itemSku,
        itemViewConfigBaseRequest.isBuyable(), itemViewConfigBaseRequest.isDiscoverable());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Error while updating item view config itemSku: {} error : {}", itemSku, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false,
        requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_VIEW_CONFIG_AND_FORCE_REVIEW, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update item view config", description =
    "update item view config for specified channel, including discoverable and buyable " + "schedule for that channel and force review flag")
  public GdnBaseRestResponse updateItemViewConfigAndForceReview(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "false") boolean forceReview,
      @RequestParam(defaultValue = "false") boolean isArchive, @RequestParam(required = false) String productSku,
      @RequestParam(defaultValue = "false") boolean scheduleRemoval,
      @RequestBody List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest) {
    ItemController.LOG.info(
      "Update itemViewConfig and force review requestId : {} and forceReview : {}", requestId,
      forceReview);
    try {
      List<Item> itemList =
          this.itemViewConfigService.updateItemViewConfigAndForceReview(storeId, itemViewConfigAndItemSkuListRequest,
              forceReview, isArchive, productSku, scheduleRemoval);
      Product product = productSearchService.updateForceReviewForProduct(storeId, itemList, forceReview, isArchive);
      itemService.publishUpdateToSolrEvent(product, itemList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false,
        requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {
    ProductApiPath.UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE}, method = {
    RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item view configuration", description = "update item view/visibility "
    + "configuration by merchantSku and merchantCode")
  @ResponseBody
  public GdnBaseRestResponse updateItemViewConfigByMerchantSkuAndMerchantCode(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam(required = false) String merchantSku, @RequestParam String merchantCode,
    @RequestBody ItemViewConfigRequest itemViewConfigRequest) {

    ItemController.LOG.info(
      "Update item view config with storeId = {}, merchantSku = {}, merchantCode = {}, " + "itemViewConfigRequest = {}", storeId, merchantSku, merchantCode,
      itemViewConfigRequest);

    try {
      this.itemViewConfigService.updateItemViewConfigByMerchantSkuAndMerchantCode(storeId, requestId, username,
        merchantSku, merchantCode, this.modelConverter.convertToItemViewConfig(itemViewConfigRequest));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE.getCode(),
        false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE}, method = {
    RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update resign merchant items", description =
    "update resign merchant items to archive, buyable & discoverable false by merchant code")
  public GdnBaseRestResponse updateResignMerchantItemsByMerchantCode(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @RequestParam String merchantCode) {

    ItemController.LOG.info("Update resign merchat items with storeId = {}, merchantCode = {}",
      storeId, merchantCode);

    try {
      this.itemService.updateResignMerchantItemsByMerchantCode(storeId, requestId, username,
        merchantCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error(e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE.getCode(), false,
        requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "gets all pristine product attribute map for different categories", description
    = "gets all pristine product attribute map for different categories")
  public GdnRestSingleResponse<PristineCategoryAttributeMapResponse> getMapForPristineCategoryAttribute(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId) throws Exception {
    MandatoryRequestParam param =
      MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    boolean isSuccess = false;
    LOG.info("trying to fetch attribute map for pristine categories for mandatoryRequestParam : {}",
      param);
    PristineCategoryAttributeMapResponse response = new PristineCategoryAttributeMapResponse();
    try {
      Map<String, String> categoryAttributeMap = itemService.getMapForPristineCategoryAttribute();
      response.setCategoryAttributeMap(categoryAttributeMap);
      isSuccess = true;
    } catch (Exception e) {
      LOG.error("failed to fetch pristine category attribute map : requestId : " + requestId, e);
    }
    return new GdnRestSingleResponse<PristineCategoryAttributeMapResponse>("", "", isSuccess,
      response, requestId);
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.UPDATE_SALES_CATALOG_FOR_PRISTINE_PRODUCT, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update salesCatalog for pristine product", description =
    "update salesCatalog " + "for pristine product")
  public GdnBaseRestResponse updateSalesCatalogForPristineProducts(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestBody(required = false) List<String> pristineIds) {
    LOG.info("starting thread to update salesCatalog for all pristine products, requestId : {}",
      requestId);
    boolean isSuccess = itemService.updateSalesCatalogForPristineProducts(pristineIds);
    return new GdnBaseRestResponse(isSuccess);
  }

  @Deprecated
  @RequestMapping(value = {
    ProductApiPath.UPDATE_ITEM_PRISTINE_DATA}, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update item pristine data", description = "update item pristine data to "
    + "prd_item collection")
  public GdnBaseRestResponse updateItemPristineData(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestBody List<PristineDataItemDto> pristineDataDtoList) {

    ItemController.LOG.info("Update item Pristine Data with storeId = {}, pristineDataDto = {}",
      storeId, pristineDataDtoList);
    boolean status = false;
    String errorMessage = StringUtils.EMPTY;
    String errorCode = StringUtils.EMPTY;
    try {
      Map<String, PristineDataItem> itemSkuToPristineDataMap = new HashMap<>();
      for (PristineDataItemDto pristineDataItemDto : pristineDataDtoList) {
        itemSkuToPristineDataMap.put(pristineDataItemDto.getPcbProductItemId(), this.modelConverter.convertToPristineDataItem(pristineDataItemDto));
      }
      this.itemService.updateItemsPristineData(storeId, itemSkuToPristineDataMap);
      status = true;
    } catch (Exception e) {
      errorCode = ProductErrorCodesEnum.UPDATE_PRISTINE_DATA.getCode();
      errorMessage = ProductErrorCodesEnum.INTERNAL_SERVER.getMessage();
      ItemController.LOG.error(
        " Error while Updating item Pristine Data with storeId = {}, pristineDataDto = {}", storeId,
        pristineDataDtoList);
      ItemController.LOG.error("Error", e);
    }
    return new GdnBaseRestResponse(errorMessage, errorCode, status, requestId);
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_ITEMS_AND_BUNDLING_INFO, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get items and bundling info by itemSku", description = "get all the items from "
    + "database and master data ")
  public GdnRestSingleResponse<ItemAndBundlingInfoResponse> getItemsAndBundlingInfo(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username, @RequestBody ItemAndBundlingInfoRequest itemAndBundlingInfoRequest) {
    try {
      ItemAndBundlingInfoVO itemAndBundlingInfoVO =
        itemService.getItemsAndBundlingInfo(storeId, channelId, clientId, requestId,
          itemAndBundlingInfoRequest.getItemSkus(), itemAndBundlingInfoRequest.getPromoBundlingIds(), username);
      ItemAndBundlingInfoResponse itemAndBundlingInfoResponse = modelConverter.convertPromoItemVOToPromoItemResponse(itemAndBundlingInfoVO);
      return new GdnRestSingleResponse<>(itemAndBundlingInfoResponse, requestId);
    } catch (Exception e) {
      log.error("#getItemsAndBundlingInfo with request {} ", itemAndBundlingInfoRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_ITEM.getCode(),
        false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_ITEM_SKUS_BY_ITEM_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all item prices for item SKUs",
    description = "get all item prices for item SKUs")
  public GdnRestListResponse<ItemPriceResponse> getItemSkusByItemCode(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String itemCode,
    @RequestParam(required = false) boolean fetchAll) {
    int size = DEFAULT_PAGE_SIZE;
    try {
      List<ItemPriceVO> itemPriceVOS = itemService.getAllItemSkuByItemCode(storeId, username, requestId, itemCode, fetchAll);
      if (CollectionUtils.isNotEmpty(itemPriceVOS)) {
        size = itemPriceVOS.size();
      }
      return new GdnRestListResponse<>(modelConverter.convertItemPriceVoToItemPriceListResponse(itemPriceVOS),
        new PageMetaData(size, 0, size), requestId);
    } catch (ApplicationRuntimeException e) {
      ItemController.LOG.error("#getItemSkusWithPriceForItemCode with request itemCode {} ",
        itemCode, e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.GET_ITEM.getMessage(),
        ProductErrorCodesEnum.GET_ITEM.getCode(), Boolean.FALSE, null, new PageMetaData(1, 0, 0),
        requestId);
    } catch (Exception ex) {
      ItemController.LOG.error("#getItemSkusWithPriceForItemCode with request itemCode {} ",
        itemCode, ex);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, null, new PageMetaData(1, 0, 0), requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_ITEM_SKUS_BY_PRISTINE_ID, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all item prices for pristine id", description = "get all item prices for "
    + "pristine id")
  public GdnRestListResponse<ItemPriceResponse> getItemSkusByPristineId(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestParam String pristineId) {
    try {
      List<ItemPriceVO> itemPriceVOS = productSearchService.getItemPriceByPristineId(storeId, username, requestId, pristineId);
      return new GdnRestListResponse<>(modelConverter.convertItemPriceVoToItemPriceListResponse(itemPriceVOS),
        new PageMetaData(itemPriceVOS.size(), 0, itemPriceVOS.size()), requestId);
    } catch (Exception ex) {
      ItemController.LOG.error("#getItemSkusWithPriceForItemCode with request pristineId {} ",
        pristineId, ex);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, null, new PageMetaData(0, 0, 0), requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get first buyable and discoverable item sku", description =
    "Get first buyable and discoverable "
      + "item sku from a sorted set of item skus by pristine id as per the buyBoxScores")
  public GdnRestSingleResponse<DefaultItemSkuResponse> getFirstBuyableDiscoverableItemSku(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username, @RequestParam String pristineId,
    @RequestBody LinkedHashSet<String> itemSkuSet) {
    DefaultItemSkuResponse response = new DefaultItemSkuResponse();
    ItemController.LOG.info(
      "getFirstBuyableDiscoverableItemSku : Get first buyable and discoverable item sku with storeId = {}, "
        + "itemSkuSet = {}, pristineId = {}", storeId, itemSkuSet, pristineId);
    try {
      MandatoryRequestParam param =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId,
          username, null);
      DefaultItemSkuVO defaultItemSkuVo =
        itemService.findFirstBuyableDiscoverableItemSkuByPristineId(storeId, itemSkuSet, pristineId);
      BeanUtils.copyProperties(defaultItemSkuVo, response);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, response,
        requestId);
    } catch (ApplicationRuntimeException e) {
      ItemController.LOG.error("#getFirstBuyableDiscoverableItemSku with itemSkuSet : {}",
        itemSkuSet, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU.getMessage(),
        ProductErrorCodesEnum.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU.getCode(), false, null,
        requestId);
    } catch (Exception e) {
      ItemController.LOG.error("#getFirstBuyableDiscoverableItemSku with itemSkuSet : {}",
        itemSkuSet, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.UPDATE_PROMOTION_PRICE_FOR_SKU_LIST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update item SKUs discount price", description = "update item SKUs discount price")
  public GdnBaseRestResponse updatePromotionPriceForSku(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody List<String> itemSkus) {
    try {
      itemService.updatePromotionPriceForItemSkuList(storeId, new HashSet<>(itemSkus));
      return new GdnBaseRestResponse(requestId);
    } catch (Exception ex) {
      ItemController.LOG.error(
        "error in updateDiscountPriceByItemSkuList with request {} and  itemSkus {} ", requestId,
        itemSkus, ex);
      return new GdnBaseRestResponse(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.UPDATE_ITEM_FLASH_SALE_ACTIVE_FLAG, method = RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update FlashSaleActiveFlag for  item", description = "Update "
    + "FlashSaleActiveFlag for item")
  public GdnBaseRestResponse updateItemFlashSaleActiveFlag(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestBody List<String> itemSkus, @RequestParam boolean isFlashSaleActive) {
    ItemController.LOG.info("Updating flashSaleActiveFlag:{} for itemskus:{}", isFlashSaleActive,
      itemSkus);
    boolean isSuccess = itemService.updateItemFlashSaleActiveFlag(storeId, itemSkus, isFlashSaleActive);
    return new GdnBaseRestResponse(isSuccess);
  }

  @RequestMapping(value = ProductApiPath.GET_ITEMS_BY_ITEMCODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch all L1 L2 L3 L4 detail by item code")
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getItemsByItemCode(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username, @PathVariable("itemCode") String itemCode) {
    ItemController.LOG.info("Fetching product and item details for itemCode : {}", itemCode);
    try {
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
        this.productSearchService.findMasterDataWithProductAndItemsInfoByItemCode(storeId, username,
          requestId, itemCode, true);
      return new GdnRestSingleResponse(null, null, true, this.modelConverter.convertToMasterDataDetailResponse(
        masterDataDetailWithProductAndItemsResponseVo), requestId);
    } catch (Exception e) {
      ItemController.LOG.error("error while fetching product and items with item code : {}",
        itemCode, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEMS_PICKUP_POINT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch item pickupPoint code by product sku")
  public GdnRestListResponse<ItemPickupPointCodeResponse> getItemPickupPointCodeByProductSku(@RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam int page,
      @RequestParam(defaultValue = "20") int size, @PathVariable("productSku") String productSku,
      @RequestParam boolean fbbActivated) {
    ItemController.LOG.info("Fetching pickupPoint codes for productSku : {}, page : {}, size : {}",
      productSku, page, size);
    try {
      Page<ItemPickupPointVo> itemPickupPointVos =
        itemSummaryService.getItemPickupPointsAndItemNameByProductSku(storeId, productSku, page,
          size, fbbActivated);
      List<ItemPickupPointCodeResponse> itemPickupPointCodeResponses =
        modelConverter.getItemPickupPointCodeResponseFromItemPickupPointVo(itemPickupPointVos.getContent());
      return new GdnRestListResponse<>(itemPickupPointCodeResponses, new PageMetaData(size, page, itemPickupPointVos.getTotalElements()), requestId);
    } catch (Exception e) {
      ItemController.LOG.error("error while fetchingfor productSku : {}, page : {}, size : {}",
        productSku, page, size, e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PICKUP_POINT_CODES, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update pickup point codes", description = "update pickup point codes")
  public GdnRestSingleResponse<SimpleLongResponse> updatePickupPoints(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody PickupPointUpdateRequest pickupPointUpdateRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointUpdateRequest.getProductSku()),
      "Product sku cannot be null");
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(pickupPointUpdateRequest.getPickupPointUpdateItemRequestList()),
      "Item list cannot be empty");
    try {
      LOG.info("Updating the pick up point for product sku : {}, request ID : {}",
        pickupPointUpdateRequest.getProductSku(), requestId);
      return new GdnRestSingleResponse<>(null, null, true, new SimpleLongResponse(
          itemService.updatePickupPoints(storeId, pickupPointUpdateRequest, false).getProduct().getVersion()),
          requestId);
    } catch (Exception ex) {
      ItemController.LOG.error(
        "error in updating the pick up point with for product Sku : {}, request ID : {} ",
        pickupPointUpdateRequest.getProductSku(), ex);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_CONTENT_CHANGE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update content change", description = "update content change")
  public GdnBaseRestResponse updateContentChange(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam String productSku, @RequestParam boolean contentChange, @RequestParam(required = false) boolean publishItems) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productSku), "Product sku cannot be null");
    try {
      LOG.info("Updating the content change for product sku : {} to {}, request ID : {}", productSku, contentChange,
        requestId);
      itemService.updateContentChange(storeId, productSku, contentChange, publishItems);
      return new GdnBaseRestResponse(requestId);
    } catch (Exception ex) {
      ItemController.LOG.error(
        "error in updating the content change for product Sku : {}, request ID : {} ", productSku,
        ex);
      return new GdnBaseRestResponse(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
        ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_IMAGES_BY_ITEM_SKUS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get item images by itemSkus", description = "Get item images by itemSkus")
  public GdnRestListResponse<ItemImagesListResponse> getListOfImagesByItemSkus(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody SimpleSetStringRequest itemSkusSet) {
    LOG.info("Get item images requestId : {}", requestId);
    List<ItemImagesListResponse> listOfItemImagesByItemSkus = new ArrayList<>();
    try {
      listOfItemImagesByItemSkus = itemSummaryService.getItemImagesListResponse(storeId, itemSkusSet.getValue());
      return new GdnRestListResponse<>(null, null, true, listOfItemImagesByItemSkus, new PageMetaData(listOfItemImagesByItemSkus.size(), 0, listOfItemImagesByItemSkus.size()),
        requestId);
    } catch (Exception e) {
      LOG.error("Get item images failed requestId : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        listOfItemImagesByItemSkus, new PageMetaData(listOfItemImagesByItemSkus.size(), 0, listOfItemImagesByItemSkus.size()),
        requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_DETAILS_BY_ITEM_CODES, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get item details by itemCodes", description = "Get item details by itemCodes")
  public GdnRestListResponse<ItemCodeDetailResponse> getItemDetailsByItemCodes(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody SimpleSetStringRequest itemCodesSet) {
    LOG.info("Get item details by itemCodes requestId : {}", requestId);
    List<ItemCodeDetailResponse> itemCodeDetailResponses = new ArrayList<>();
    try {
      itemCodeDetailResponses = itemService.getItemDetailsByItemCodes(storeId, itemCodesSet.getValue());
      return new GdnRestListResponse<>(null, null, true, itemCodeDetailResponses, new PageMetaData(itemCodeDetailResponses.size(), 0, itemCodeDetailResponses.size()),
        requestId);
    } catch (Exception e) {
      LOG.error("Get item details failed by itemCodes  : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        itemCodeDetailResponses, new PageMetaData(itemCodeDetailResponses.size(), 0, itemCodeDetailResponses.size()),
        requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_TYPE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product type", description = "update product type")
  public GdnBaseRestResponse updateProductType(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody ProductTypeEditRequest request) {
    ItemController.LOG.info("Update edited product with productRequest = {}", new Object[] {request});
    try {
      this.itemService.updateProductTypeOrContentChange(request, storeId);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (SolrCustomException e) {
      ItemController.LOG.error("#updateSpecialAttributes failed for productCode : {} ",
        request.getProductCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
    catch (Exception e) {
      ItemController.LOG.error("#updateSpecialAttributes failed for productCode : {} ", request.getProductCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_PRODUCT_SPECIAL_ATTRIBUTES_FAILED.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_ITEMS_PICKUP_POINT_CODE_BY_ITEM_SKU, method =
    RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch item pickupPoint code by item skus")
  public GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody SimpleListStringRequest itemSkusList) {
    log.info("Fetching pickupPoint codes for itemSku : {}", itemSkusList);
    try {
      List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponses =
        itemPickupPointService.findPickUpPointCodeByItemSkuInAndDelivery(storeId, itemSkusList.getValue(), true);
      return new GdnRestListResponse<>(null, null, true, itemSkuPickupPointCodeResponses,
        new PageMetaData(itemSkuPickupPointCodeResponses.size(), 0, itemSkuPickupPointCodeResponses.size()), requestId);
    } catch (Exception e) {
      log.error("error while fetching for itemSkus : {}", itemSkusList, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.REPUBLISH_ITEM_PICKUP_POINT_TO_AGP, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "republish items pickup_point data to AGP")
  public GdnBaseRestResponse republishItemPickupPointToAgp(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @RequestParam(required = false, defaultValue = "true") boolean republishToAgp,
    @RequestBody List<ItemPickupPointRequest> itemPickupPointRequestList) {
    ItemController.log.info(
      "Publishing a new L5 event to AGP for ItemSKUs and PickupPoint " + "request: {}",
      itemPickupPointRequestList);
    try {
      asyncProcessor.submitWithBackoff(AgpConstant.COMMAND_REPUBLISH_ITEMS_TO_AGP,
        () -> this.itemPickupPointService.republishItemPickupPointToAgp(itemPickupPointRequestList,
          storeId, republishToAgp));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ItemController.LOG.error("error while submitting data with error as : ", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }
  @RequestMapping(value = ProductApiPath.GET_L4_ITEM_LIST_BY_PRODUCT_SKU, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get L4 Item List By ProductSku", description = "Item summary response to be "
    + "consumed by pcu-external")
  public GdnRestListResponse<ItemLevel4ListingResponse> getL4ItemListByProductSku(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(defaultValue = "0") Integer page, @RequestParam(required = false) Integer size,
    @RequestBody ItemLevel4ListingWebRequest request) {
    try {
      Page<ItemLevel4ListingResponse> listingResponsePage =
          itemService.getL4ItemListByProductSku(request.getProductSkus(), storeId, page, size);
      return new GdnRestListResponse<>(null, null, Boolean.TRUE, listingResponsePage.getContent(),
          new PageMetaData(listingResponsePage.getPageable().getPageSize(),
              listingResponsePage.getPageable().getPageNumber(), listingResponsePage.getTotalElements()),
        requestId);
    } catch (Exception e) {
      log.error("Error while getting L4 item summary with ProductSku: {} error : {}", request.getProductSkus(),
        e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_BASIC_ITEMS_DETAILS_PAGINATION,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Retrieve L4 item list by product SKU",
      description = "Returns a lightweight L4 item summary response for consumption by external PCU services.")
  public GdnRestListResponse<ItemBasicL4Response> getItemL4Lite(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(required = false) Integer size,
      @RequestBody ItemLevel4ListingWebRequest request) {
    try {
      var response = itemService.getL4ItemListByProductSkuLite(request.getProductSkus(), storeId, page, size);
      return new GdnRestListResponse<>(null, null, Boolean.TRUE, response.getContent(),
          new PageMetaData(response.getPageable().getPageSize(),
              response.getPageable().getPageNumber(), response.getTotalElements()),
          requestId);

    } catch (Exception e) {
      log.error("Error while getting L4 item summary with ProductSku: {} error : {}", request.getProductSkus(),
          e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.GET_BASIC_ITEM_DETAILS}, method = {
    RequestMethod.GET}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get basic item details", description = "get basic item details")
  public GdnRestSingleResponse<ItemBasicDetailResponse> getBasicItemDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size, @PathVariable("itemSku") String itemSku) throws Exception {
    log.info("getBasicItemDetails with itemSku : {} ");
    try {
      ItemBasicDetailResponse productTypeAndPPCodeResponses =
          itemPickupPointService.getBasicItemDetails(storeId, page, size, itemSku);
      return new GdnRestSingleResponse<>(null, null, true, productTypeAndPPCodeResponses, requestId);
    } catch (Exception e) {
      log.error("Error while fetching itemPickupPoint by itemRequest {} ", itemSku, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_L5_ITEM_LIST_BY_PRODUCT_SKU, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get L5 Item List By ProductSku", description = "Item summary response to be "
    + "consumed by pcu-external")
  public GdnRestListResponse<ItemLevel5Response> getL5ItemListByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestBody ItemLevel4ListingWebRequest request,
      @RequestParam(required = false, defaultValue = "false") boolean fetchProductData,
      @RequestParam(required = false, defaultValue = "false") boolean fetchB2bData,
      @RequestParam(required = false, defaultValue = "false") boolean fetchCategoryData,
      @RequestParam(required = false, defaultValue = "false") boolean excludeDistributionPickupPoint,
      @RequestParam(required = false) String fetchViewConfigByChannel) {
    try {
      List<String> productSkusList =
          Optional.ofNullable(request.getProductSkus()).map(ArrayList::new).orElseGet(ArrayList::new);
      List<ItemLevel5Response> listingL5Response =
          itemService.getL5ItemListByProductSku(storeId, productSkusList, fetchProductData,
              request.getPickupPointCodes(), request.getPromoTypes(), fetchB2bData, fetchCategoryData,
              fetchViewConfigByChannel, excludeDistributionPickupPoint);
      return new GdnRestListResponse<>(null,
          null, Boolean.TRUE, listingL5Response, new PageMetaData(listingL5Response.size(), 0, listingL5Response.size()), requestId);
    } catch (Exception e) {
      log.error("Error while getting L5 item summary with ProductSku: {} error : {}", request.getProductSkus(),
          e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_DEFAULT_PICKUP_POINT_CODE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get default pickup point code", description = "get default pickup point code")
  public GdnRestListResponse<ItemSkuPickupPointResponse> getDefaultPickupPointCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody DefaultPickupPointRequest request) {
    try {
      log.info("getting default pickup point codes for item-skus  : {} and republish : {} ",
          request.getSkus(), request.isRepublish());
      List<ItemSkuPickupPointResponse> itemSkuPickupPointResponses = itemPickupPointService.
          getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(storeId, request.getSkus(), request.isRepublish());
      return new GdnRestListResponse<>(null, null, Boolean.TRUE,
          itemSkuPickupPointResponses, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting default pickup point codes for item-skus: {} ", request.getSkus(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get item images by itemSkus", description = "Get item images by itemSkus")
  public GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productSku) {
    LOG.info("Get item images requestId : {} ", requestId);
    List<ItemBasicDetailV2Response> itemImagesListResponses = new ArrayList<>();
    try {
      itemImagesListResponses = itemSummaryService.getItemBasicDetailsByProductSku(storeId, productSku);
      return new GdnRestListResponse<>(null, null, true, itemImagesListResponses,
          new PageMetaData(itemImagesListResponses.size(), 0, itemImagesListResponses.size()), requestId);
    } catch (Exception e) {
      LOG.error("Get item images failed requestId : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          itemImagesListResponses, new PageMetaData(itemImagesListResponses.size(), 0, itemImagesListResponses.size()),
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get item details for given itemSkus", description = "Get basic item details for given itemSkus")
  public GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "false") boolean fetchBundleRecipe,
      @RequestBody SimpleListStringRequest itemSkus) {
    LOG.info("Get item details for requestId : {} ", requestId);
    List<ItemBasicDetailV2Response> itemBasicDetailV2Responses = new ArrayList<>();
    try {
      itemBasicDetailV2Responses =
          itemSummaryService.getBulkItemDetailsByItemSkus(storeId, fetchBundleRecipe, itemSkus.getValue());
      return new GdnRestListResponse<>(null, null, true, itemBasicDetailV2Responses,
          new PageMetaData(itemBasicDetailV2Responses.size(), 0, itemBasicDetailV2Responses.size()), requestId);
    } catch (Exception e) {
      LOG.error("Get item details failed where requestId : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          itemBasicDetailV2Responses,
          new PageMetaData(itemBasicDetailV2Responses.size(), 0, itemBasicDetailV2Responses.size()), requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_ITEM_CODES, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get item images by itemCodes", description = "Get item images by itemCodes")
  public GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetailsByItemCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody SimpleListStringRequest itemCodesList) {
    LOG.info("Get item images requestId : {} ", requestId);
    List<ItemBasicDetailV2Response> itemImagesListResponses = new ArrayList<>();
    try {
      itemImagesListResponses = itemSummaryService.getItemBasicDetailsByItemCodes(storeId, itemCodesList.getValue());
      return new GdnRestListResponse<>(null, null, true, itemImagesListResponses,
          new PageMetaData(itemImagesListResponses.size(), 0, itemImagesListResponses.size()), requestId);
    } catch (Exception e) {
      LOG.error("Get item images failed requestId : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          itemImagesListResponses, new PageMetaData(itemImagesListResponses.size(), 0, itemImagesListResponses.size()),
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_ITEM_SKUS, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get item basic details by itemSkus", description = "Get item basic details by itemSkus")
  public GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetailsByItemSkus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam boolean inAllProducts,
      @RequestParam(required = false, defaultValue = "false") boolean needProductData,
      @RequestParam(required = false, defaultValue = "false") boolean needCategoryData,
      @RequestParam(required = false, defaultValue = "true") boolean needFbbFlag,
      @RequestParam(required = false, defaultValue = "true") boolean needAttributeData,
      @RequestBody SimpleListStringRequest itemSkusList) {
    LOG.info("Get item basic details for requestId : {} ", requestId);
    List<ItemBasicDetailV2Response> itemBasicDetailsListResponses = new ArrayList<>();
    try {
      itemBasicDetailsListResponses =
          itemSummaryService.getItemBasicDetailsByItemSkus(storeId, inAllProducts, needProductData, needCategoryData,
              itemSkusList.getValue(), needFbbFlag, needAttributeData);
      return new GdnRestListResponse<>(null, null, true, itemBasicDetailsListResponses,
          new PageMetaData(itemBasicDetailsListResponses.size(), 0, itemBasicDetailsListResponses.size()), requestId);
    } catch (Exception e) {
      LOG.error("Get item basic details failed requestId : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          itemBasicDetailsListResponses,
          new PageMetaData(itemBasicDetailsListResponses.size(), 0, itemBasicDetailsListResponses.size()), requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_SHARED_PRODUCT_BUNDLE_RECIPE_BY_ITEM_CODES, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get item basic details by itemSkus", description = "Get item basic details by itemSkus")
  public GdnRestListResponse<SharedProductBundleRecipeResponse> getSharedProductBundleRecipeDetails(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest request) {
    LOG.info("Get shared product bundle details. request : {} ", request);
    try {
      List<SharedProductBundleRecipeResponse> sharedProductBundleRecipeResponses =
          itemService.getBundleRecipeForSharedItems(storeId, request.getValue());
      return new GdnRestListResponse<>(null, null, true, sharedProductBundleRecipeResponses,
          new PageMetaData(sharedProductBundleRecipeResponses.size(), 0, sharedProductBundleRecipeResponses.size()),
          requestId);
    } catch (Exception e) {
      LOG.error("Get item basic details failed requestId : {} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, new ArrayList<>(),
          new PageMetaData(0, 0, 0), requestId);
    }
  }

  @GetMapping(value = ProductApiPath.FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODES, produces =
    {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get item basic details by Item Codes", description = "Get item basic "
    + "details by Item Codes")
  public GdnRestListResponse<ItemCodeBasicDetailResponse> fetchBasicItemDetailsByItemCodes(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size,
    @RequestParam String sortBy, @RequestParam String orderBy, @RequestParam String itemCode,
    @RequestParam(name = "searchKey", required = false, defaultValue = StringUtils.EMPTY) String searchKey) {
    LOG.info("Get item basic details for requestId : {} ", requestId);
    List<ItemCodeBasicDetailResponse> itemBasicDetailsListResponses = new ArrayList<>();
    try {
      itemBasicDetailsListResponses =
        itemSummaryService.fetchBasicItemDetailsByItemCodes(storeId, itemCode, searchKey, page,
          size, sortBy, orderBy).getContent();
      return new GdnRestListResponse<>(null, null, true, itemBasicDetailsListResponses,
        new PageMetaData(itemBasicDetailsListResponses.size(), 0, itemBasicDetailsListResponses.size()), requestId);
    } catch (Exception e) {
      LOG.error("Get item basic details failed itemCode : {} ", itemCode, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, itemBasicDetailsListResponses,
        new PageMetaData(itemBasicDetailsListResponses.size(), 0, itemBasicDetailsListResponses.size()), requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_UPC_CODE_STATUS, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get upc code status", description =
      "for a list of upc code check whether they already exist across a merchant")
  public GdnRestListResponse<UpcStatusResponse> getUpcStatus(@RequestParam String storeId, @RequestParam String requestId, @RequestParam(required = false) String username ,
      @RequestBody UpcStatusRequest upcStatusRequest) {
    log.info("Get upc details for requestId : {} ", requestId);
    List<UpcStatusResponse> upcStatusResponses = new ArrayList<>();
    try {
      return new GdnRestListResponse<>(null, null, true,
          itemService.fetchUpcCodeStatus(storeId, upcStatusRequest.getMerchantCode(),
              upcStatusRequest.getUpcCodes()),
          new PageMetaData(upcStatusResponses.size(), 0, upcStatusResponses.size()), requestId);
    } catch (Exception e) {
      log.error("Get upc details for requestId : {}, error - ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          Collections.emptyList(),
          new PageMetaData(upcStatusResponses.size(), 0, upcStatusResponses.size()), requestId);
    }
  }
}
