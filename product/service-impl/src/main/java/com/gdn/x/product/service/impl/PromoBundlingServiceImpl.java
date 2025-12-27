package com.gdn.x.product.service.impl;

import com.gdn.partners.product.pricing.web.model.promo.bundling.request.ItemInfoRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.outbound.api.ProductPricingOutbound;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.PromoBundlingService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.promotion.enums.PromoBundlingType;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

/**
 * Created by w.william on 3/9/2018.
 */
@Service
public class PromoBundlingServiceImpl implements PromoBundlingService {

  private static final Logger LOG = LoggerFactory.getLogger(PromoBundlingServiceImpl.class);

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private PromotionOutbound promotionOutbound;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  @Lazy
  private ItemPriceService itemPriceService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private SystemParameterService systemParameterService;

  @Override
  public ComboResponseVO getActiveCombos(String storeId,
      String channelId, String clientId, String requestId, String username,
      ActiveComboRequestVO activeComboRequestVO, int page, int size, String sortBy, String sortType)
      throws Exception{
    checkArgument(
        StringUtils.isNotBlank(activeComboRequestVO.getItemCode())
            || StringUtils.isNotBlank(activeComboRequestVO.getItemSku())
            || StringUtils.isNotBlank(activeComboRequestVO.getPristineId()),
        "activeComboRequestVO must not be blank");

    ActivePromoBundlingResponseVO activePromoBundlingResponseVO;
    if (StringUtils.isNotBlank(activeComboRequestVO.getPristineId())) {
      activePromoBundlingResponseVO =
          getActiveCombosByPristineId(activeComboRequestVO.getPristineId(), storeId, channelId,
              clientId, requestId, username, page, size, sortBy, sortType);
    } else if (StringUtils.isNotBlank(activeComboRequestVO.getItemCode())) {
      activePromoBundlingResponseVO =
          getPromoBundlingDetailResponseByItemCode(activeComboRequestVO.getItemCode(), storeId,
              channelId, clientId, requestId, username, page, size, sortBy, sortType);
    } else {
      activePromoBundlingResponseVO =
          getPromoBundlingByItemSku(activeComboRequestVO, storeId, channelId, clientId, requestId,
              username, page, size, sortBy, sortType);
    }

    List<ComboVO> comboRuleResults = new ArrayList<>();
    activePromoBundlingResponseVO.getPromoBundlingDetailResponseVOList().stream()
        .forEach(promoBundlingDetailResponseVO -> {
          ComboVO comboVO =
              convertPromoBundlingDetailResponseVoToComboVO(storeId, requestId, username,
                  promoBundlingDetailResponseVO);
          comboRuleResults.add(comboVO);
        });

    ComboResponseVO comboResponseVO = new ComboResponseVO();
    comboResponseVO.setComboList(comboRuleResults);
    comboResponseVO.setTotalRecords(activePromoBundlingResponseVO.getTotalRecords());

    return comboResponseVO;
  }

  private ActivePromoBundlingResponseVO getPromoBundlingByItemSku(
      ActiveComboRequestVO activeComboRequestVO, String storeId, String channelId, String clientId,
      String requestId, String username, int page, int size, String sortBy, String sortType)
      throws Exception {
    Item item = this.itemService
        .getItem(storeId, requestId, username, activeComboRequestVO.getItemSku(), true, false, false, false, null,
            false, false);
    if (item.isSynchronized()) {
      if (item.getPristineDataItem() != null) {
        return getActiveCombosByPristineId(item.getPristineDataItem().getPristineId(), storeId,
            channelId, clientId, requestId, username, page, size, sortBy, sortType);
      } else if (item.getItemCode() != null) {
        return getPromoBundlingDetailResponseByItemCode(item.getItemCode(), storeId, channelId,
            clientId, requestId, username, page, size, sortBy, sortType);
      }
    }
    return getPromoBundlingDetailResponseByItemSku(activeComboRequestVO.getItemSku(), storeId,
        channelId, clientId, requestId, username, page, size, sortBy, sortType);
  }

  private ActivePromoBundlingResponseVO getActiveCombosByPristineId(String pristineId,
      String storeId, String channelId, String clientId, String requestId, String username, int page, int size,
      String sortBy, String sortType) throws Exception {

    Set<String> itemCodes = itemService.getItemCodesByPristineId(storeId, pristineId);
    ActivePromoBundlingResponseVO activePromoBundlingResponseVO =
        this.promotionOutbound.getActiveCombosByItemCodes(MandatoryRequestParam
                .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null), page, size,
            sortBy, sortType, itemCodes);

    return activePromoBundlingResponseVO;
  }

  private ActivePromoBundlingResponseVO getPromoBundlingDetailResponseByItemCode(
      String itemCode, String storeId, String channelId, String clientId, String requestId, String username,
      int page, int size, String sortBy, String sortType) throws Exception {

    Set<String> itemCodes = new HashSet<>(Arrays.asList(itemCode));
    ActivePromoBundlingResponseVO activePromoBundlingResponseVO =
        this.promotionOutbound.getActiveCombosByItemCodes(MandatoryRequestParam
                .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null), page, size,
            sortBy, sortType, itemCodes);

    return activePromoBundlingResponseVO;
  }

  private ActivePromoBundlingResponseVO getPromoBundlingDetailResponseByItemSku(String itemSku,
      String storeId, String channelId, String clientId, String requestId, String username, int page, int size, String sortBy,
      String sortType) throws Exception {

    Set<String> itemSkus = new HashSet<>(Arrays.asList(itemSku));
    ActivePromoBundlingResponseVO activePromoBundlingResponseVO = this.promotionOutbound
        .getActiveByPromoBundlingTypeAndItemSkus(MandatoryRequestParam
                .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null),
            PromoBundlingType.COMBO, itemSkus);

    return activePromoBundlingResponseVO;
  }

  private ComboVO convertPromoBundlingDetailResponseVoToComboVO(String storeId, String requestId,
      String username, PromoBundlingDetailResponseVO promoBundlingDetailResponseVO) {

    ComboVO comboVO = objectConverterService
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);
    try {
      comboVO.setComboItems(convertPromoBundlingDetailResponseToComboItemVO(storeId, requestId,
          username, promoBundlingDetailResponseVO));
    } catch (Exception e) {
      LOG.error("failed to convert promoBundlingDetailResponseVO to promoBundlingItemVO, {}",
          promoBundlingDetailResponseVO, e);
    }

    return comboVO;
  }

  private List<ComboItemVO> convertPromoBundlingDetailResponseToComboItemVO(String storeId,
      String requestId, String username,
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO) throws Exception {

    if(CollectionUtils.isEmpty(promoBundlingDetailResponseVO.getComboRules())){
      return Collections.emptyList();
    }

    List<ComboItemVO> comboItemVOs = objectConverterService
        .convertComboRulesResponseToComboItemVO(promoBundlingDetailResponseVO.getComboRules());

    Set<String> itemSkus = new HashSet<>();
    comboItemVOs.stream().forEachOrdered(comboItemVO -> {
      itemSkus.add(comboItemVO.getItemSku());
    });

    List<ProductAndItemsVO> productAndItemsVOS =
        productService.getProductAndItemsByItemSkusForActiveItems(storeId, requestId, username, itemSkus, true, false, false, false);

    Map<String, Product> productsByProductSku =
        productAndItemsVOS.stream().map(ProductAndItemsVO::getProduct)
            .collect(Collectors.toMap(Product::getProductSku, Function.identity(), (oldValue, newValue) -> newValue));

    Map<String, Item> itemsByItemSku = productAndItemsVOS.stream()
        .flatMap(productAndItemsVO -> productAndItemsVO.getItems().stream())
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (oldValue, newValue) -> newValue));

    comboItemVOs.stream().forEach(comboItemVO -> {
      try {
        Item item = itemsByItemSku.get(comboItemVO.getItemSku());
        Product product = productsByProductSku.get(item.getProductSku());

        objectConverterService.convertItemAndProductToComboItemVO(comboItemVO, item, product);
        objectConverterService.convertProductToComboItemVO(comboItemVO, product);
        double finalPrice = this.itemPriceService.getFinalPrice(
            item.getPrice().stream().findFirst().orElse(new Price()), comboItemVO.getDiscountPercentage());
        comboItemVO.setFinalPrice(finalPrice);

      } catch (Exception e) {
        LOG.error("failed to convert masterDataItem to comboItemVO, {}", comboItemVO, e);
      }
    });

    return comboItemVOs;
  }

  @Override
  public WholesaleVO getWholesaleByItemSku(String storeId, String channelId, String clientId, String requestId,
      String username, String itemSku, boolean pristine, boolean includeNewBundlings) throws Exception {
    checkArgument(StringUtils.isNotBlank(itemSku), "itemSku must not be blank");
    Item item = this.itemService.getItem(storeId, requestId, username, itemSku, true, true, false,
        false, null, false, false);
    Set<String> itemCodes = new HashSet<>(Arrays.asList(item.getItemCode()));
    if(item.getPristineDataItem() != null && pristine){
      itemCodes
          .addAll(this.itemService.getItemCodesByPristine(storeId, item.getPristineDataItem()));
    }
    PromoBundlingByItemSkuAndItemCodesResponseVO wholesaleResponse =
        this.promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(
            MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId,
                requestId, username, null),
            PromoBundlingType.WHOLESALE.name(), itemSku, itemCodes, includeNewBundlings);

    return convertPromoBundlingDetailResponseToWholesaleVO(
        wholesaleResponse, item);
  }

  private WholesaleVO convertPromoBundlingDetailResponseToWholesaleVO(
      PromoBundlingByItemSkuAndItemCodesResponseVO wholesaleResponse, Item item)
      throws Exception {
    if(!(wholesaleResponse==null)){
      Price price = item.getPrice().stream().findFirst().orElse(new Price());
      WholesaleVO wholesaleVO = this.objectConverterService
          .convertPromoBundlingDetailResponseToWholesaleVO(wholesaleResponse, item);

      List<WholesaleRuleVO> wholesaleRuleVOList = this.objectConverterService
          .convertPromoBundlingDetailResponseToWholesaleRuleVO(wholesaleResponse.getPromoBundling());

      wholesaleVO.setWholesaleRules(wholesaleRuleVOList);
      wholesaleVO.setTotal(wholesaleResponse.getTotalWholesaleRule());
      wholesaleVO.getWholesaleRules().stream().forEach(wholesaleRuleVO -> {
        wholesaleRuleVO.setFinalPrice(
            this.itemPriceService.getFinalPrice(price, wholesaleRuleVO.getDiscountPercentage()));
      });
      return wholesaleVO;
    } else {
      return new WholesaleVO();
    }
  }

  @Override
  public ComboDetailVo getComboDetailByItemSku(String storeId, String channelId, String clientId, String requestId,
      String username, String itemSku, boolean isPristine, boolean includeNewBundlings) throws Exception {

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, username);
    Item item = itemService.getItem(storeId, requestId, username, itemSku, true, false, false, false, null, false, false);
    if (item.getPristineDataItem() != null && isPristine) {
      Set<String> itemCodes =
          this.itemService.getItemCodesByPristine(storeId, item.getPristineDataItem());
      return constructPromoBundlingDetailVo(itemSku, itemCodes, mandatoryRequestParam, includeNewBundlings);
    }

    return constructPromoBundlingDetailVo(itemSku, Collections.singleton(item.getItemCode()),
        mandatoryRequestParam, includeNewBundlings);
  }

  private ComboDetailVo constructPromoBundlingDetailVo(String itemSku,
      Set<String> itemCodes, MandatoryRequestParam param, boolean newPromoBundlingVersion) throws Exception {
    PromoBundlingByItemSkuAndItemCodesResponseVO
        promoBundlingByItemSkuAndItemCodesResponseVO =
        promotionOutbound.getActiveAndPromoBundlingTotalByPromoBundlingType(param,
            PromoBundlingType.COMBO.name(), itemSku, itemCodes, newPromoBundlingVersion);

    return toPromoBundlingDetailVo(param.getStoreId(), param.getRequestId(), param.getUsername(),
        promoBundlingByItemSkuAndItemCodesResponseVO);
  }

  private ComboDetailVo toPromoBundlingDetailVo(String storeId, String requestId, String username,
      PromoBundlingByItemSkuAndItemCodesResponseVO promoBundlingByItemSkuAndItemCodesResponseVO)
      throws Exception {
    ComboDetailVo comboDetailVo = new ComboDetailVo();
    if(!(promoBundlingByItemSkuAndItemCodesResponseVO==null)){
      PromoBundlingDetailResponseVO promoBundlingDetail =
          promoBundlingByItemSkuAndItemCodesResponseVO.getPromoBundling();
      comboDetailVo
          .setTotal(promoBundlingByItemSkuAndItemCodesResponseVO.getTotalComboRule());
      comboDetailVo.setEndDate(promoBundlingDetail.getEndDate());
      comboDetailVo.setStartDate(promoBundlingDetail.getStartDate());
      comboDetailVo.setPromoBundlingId(promoBundlingDetail.getPromoBundlingId());
      comboDetailVo.setPromoBundlingType(promoBundlingDetail.getPromoBundlingType());
      comboDetailVo.setComboItems(convertPromoBundlingDetailResponseToComboItemVO(storeId, requestId,
          username, promoBundlingDetail));
      comboDetailVo.setPromoBundlingName(promoBundlingDetail.getPromoBundlingName());
    }
    return comboDetailVo;
  }

  @Override
  public List<PromoBundlingDetailResponseVO> getByPromoBundlingIds(MandatoryRequestParam param,
      Set<String> promoBundlingIds, Set<String> itemSkus) {
    if (CollectionUtils.isNotEmpty(promoBundlingIds)) {
      return promotionOutbound.getPromoBundlingDetailByPromoBundlingIds(param, promoBundlingIds, itemSkus);
    } else {
      return new ArrayList<>();
    }
  }

  @Override
  public Map<String, List<WholesaleRuleVO>> getWholesaleRules(MandatoryRequestParam mandatoryRequestParam,
      List<ItemPickupPoint> itemPickupPointList) {
    Set<ItemInfoRequest> itemInfoRequests = itemPickupPointList.stream().filter(
            itemPickupPoint -> CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings()) && (
                itemPickupPoint.getActivePromoBundlings().contains(Constants.WHOLESALE)
                    || itemPickupPoint.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE)))
        .map(itemPickupPoint -> new ItemInfoRequest(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()))
        .collect(Collectors.toSet());

    if (CollectionUtils.isNotEmpty(itemInfoRequests)) {
      List<PromoBundlingSkuDetailResponse> promoBundlingSkuDetailResponseList =
          productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(mandatoryRequestParam,
              PromoBundlingType.WHOLESALE.name(), itemInfoRequests);
      Map<String, List<WholesaleRuleVO>> promoDetailResponseMap = new HashMap<>();
      promoBundlingSkuDetailResponseList.forEach(promoBundlingSkuDetailResponse -> promoDetailResponseMap.put(
          CommonUtil.generatePickupPointKey(promoBundlingSkuDetailResponse.getItemSku(),
              promoBundlingSkuDetailResponse.getPickupPointCode()),
          ProductAndItemsUtil.toWholesaleRuleVO(promoBundlingSkuDetailResponse.getWholesaleRules())));
      double minimumPrice = Double.parseDouble(
          systemParameterService.findValueByStoreIdAndVariable(mandatoryRequestParam.getStoreId(),
              SystemParameterNames.MINIMUM_PRICE).getValue());
      return constructWholesaleRules(itemPickupPointList, promoDetailResponseMap, minimumPrice);
    } else {
      return new HashMap<>();
    }
  }

  private Map<String, List<WholesaleRuleVO>> constructWholesaleRules(List<ItemPickupPoint> itemPickupPointList,
      Map<String, List<WholesaleRuleVO>> promoDetailResponseMap, Double minimumPrice) {
    Map<String, List<WholesaleRuleVO>> wholesaleRuleVOMap = new HashMap<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      if (promoDetailResponseMap.containsKey(
          CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()))) {
        List<WholesaleRuleVO> wholesaleRuleVOS = promoDetailResponseMap.get(
            CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()));
        wholesaleRuleVOS.forEach(wholesaleRuleVO -> wholesaleRuleVO.setFinalPrice(
            this.itemPriceService.getFinalPriceWithMinimumPriceParameter(
                itemPickupPoint.getPrice().stream().findFirst().orElse(new Price()),
                wholesaleRuleVO.getDiscountPercentage(), minimumPrice)));
        wholesaleRuleVOMap.put(
            CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()),
            wholesaleRuleVOS);
      } else {
        wholesaleRuleVOMap.put(
            CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()),
            new ArrayList<>());
      }
    }
    return wholesaleRuleVOMap;
  }
}
