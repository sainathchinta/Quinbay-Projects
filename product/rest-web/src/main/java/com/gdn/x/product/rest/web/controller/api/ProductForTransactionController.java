package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSkuVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemSkuDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;
import com.gdn.x.product.rest.web.model.response.SimpleItemResponse;
import com.gdn.x.product.service.api.ProductForTransactionService;
import com.gdn.x.product.service.api.SystemParameterService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.List;
import java.util.Map;

@RestController
@Tag(name = "Transaction Controller",
  description = "Product Search related to transactions process API")
@Slf4j
public class ProductForTransactionController {

  private static final Logger LOG = LoggerFactory.getLogger(ProductForTransactionController.class);

  @Autowired
  private ProductForTransactionService productForTransactionService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private SystemParameterService systemParameterService;

  @RequestMapping(value = {ProductApiPath.GET_PRICE_BY_ITEM_SKU}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<SimpleItemResponse> getBuyableAndOfferPriceByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam Map<String, String> requestParams, @RequestParam String channel,
      @RequestBody SimpleListStringRequest itemSkuList) {
    ProductForTransactionController.LOG.debug(
        "getProductPriceByItemSku with req params:{}, req body:{}", requestParams, itemSkuList);
    List<ItemPriceVO> itemSku;
    try {
      itemSku =
          this.productForTransactionService.getProductPriceForTransaction(storeId,
              itemSkuList.getValue(), channel);
      return this.modelConverter.convertToItemPriceRestWebGdnRestListResponse(itemSku, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductForTransactionController.LOG.error(
          "#getProductPriceByItemSku error with itemSkus = {}, with error = {}", itemSkuList,
          e.getMessage());
      return new GdnRestListResponse<SimpleItemResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_PRICE_BY_ITEM_SKU.getCode(), false, requestId);
    } catch (Exception e) {
      ProductForTransactionController.LOG.error("#getProductPriceByItemSku error with itemSkus {}",
          itemSkuList, e);
      return new GdnRestListResponse<SimpleItemResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_PRICE_BY_ITEM_SKU.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.FILTER_PRICE_BY_ONLINE_AND_OFFLINE_ITEM_SKU}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Filter buyable and offer price by online and offline item SKU",
      description = "Called by X-Cart for Seoul Product Detail", deprecated = true)
  public GdnRestListResponse<SimpleItemResponse> filterBuyableAndOfferPriceByOnlineAndOfflineItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam Map<String, String> requestParams, @RequestParam String channel,
      @RequestBody List<ItemSkuDTO> itemSkuDTOList) {
    ProductForTransactionController.LOG.debug(
        "filterBuyableAndOfferPriceByOnlineAndOfflineItemSku with req params:{}, req body:{}",
        requestParams, itemSkuDTOList);

    List<ItemSkuVO> itemSkuVOList = this.modelConverter.convertRequestListToModel(itemSkuDTOList, ItemSkuVO.class);

    try {
      List<ItemPriceVO> itemSku =
          this.productForTransactionService.findProductPriceForOnlineAndOfflineTransaction(storeId,
              itemSkuVOList, channel, requestId, username);
      return this.modelConverter.convertToItemPriceRestWebGdnRestListResponse(itemSku, requestId);
    } catch (ApplicationRuntimeException e) {
      ProductForTransactionController.LOG.error(
          "#filterBuyableAndOfferPriceByOnlineAndOfflineItemSku error with itemSkuDTOList = {}", itemSkuDTOList, e);
      return new GdnRestListResponse<SimpleItemResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_PRICE_BY_ITEM_SKU.getCode(), false, requestId);
    } catch (Exception e) {
      ProductForTransactionController.LOG.error(
          "#filterBuyableAndOfferPriceByOnlineAndOfflineItemSku error with itemSkuDTOList {}", itemSkuDTOList, e);
      return new GdnRestListResponse<SimpleItemResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_PRICE_BY_ITEM_SKU.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.GET_PRODUCT_BY_ITEM_SKU}, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ProductForTransactionResponse> getProductForTransactionByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest itemSkus) {
    log.info("Fetch info for transaction for skus : {}", itemSkus.getValue());
    try {
      List<ProductForTransactionVO> productVOs =
        this.productForTransactionService.findProductForTransactionByItemSkus(storeId, requestId, username,
          itemSkus.getValue());
      return new GdnRestListResponse<>(this.modelConverter.convertToProductForTransactionResponse(productVOs), null,
        requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("#getProductForTransactionByItemSku error with itemSkus = {}, error - ", itemSkus,
        e);
      return new GdnRestListResponse<>(e.getMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_FOR_TRANSACTION_BY_ITEM_SKU.getCode(), false,
        Collections.emptyList(), null, requestId);
    } catch (Exception e) {
      log.error("#getProductForTransactionByItemSku error with itemSkus {}, error - ", itemSkus, e);
      return new GdnRestListResponse<>(e.getMessage(),
        ProductErrorCodesEnum.GET_PRODUCT_FOR_TRANSACTION_BY_ITEM_SKU.getCode(), false,
        Collections.emptyList(), null, requestId);
    }
  }
}
