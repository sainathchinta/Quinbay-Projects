package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.CacheItemVO;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.model.vo.PristineSimilarItemVo;
import com.gdn.x.product.service.PristineService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.util.ProductAttributesUtil;

/**
 * Created by keshashah on 18/12/17.
 */
@Service
public class PristineServiceImpl implements PristineService {

  private static final String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristineId must not be blank";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "store id must not be blank";

  private static final String PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU_MUST_NOT_BE_BLANK
      = "pristineIdOrProductCodeOrSku must not be blank";

  private static final String DEFAULT_PRICE_CHANNEL = "DEFAULT";

  @Autowired
  private PristineCacheableService pristineCacheableService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Autowired
  private PristineItemRepository pristineItemRepository;

  @Autowired
  private ItemCacheableService itemCacheableService;

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private ProductAttributesUtil productAttributesUtil;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Override
  public PristineItemDetailAndMappingVo getPristineItemsMappingByPristineId(
      MandatoryRequestParam param, String pristineId, String defaultSku) throws Exception {
    PristineItemDetailAndMappingVo mappingVo = new PristineItemDetailAndMappingVo();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(param.getStoreId(), pristineId);
    PristineDataItem pristineItem = pristineItemAndSiblingsVO.getPristineDataItem();
    if (pristineItem != null) {
      List<PristineDataItem> pristineItemList = pristineItemAndSiblingsVO.getSiblings();
      if (StringUtils.isNotBlank(defaultSku)) {
        mappingVo = getItemAndBuildResponseByDefaultSku(param, defaultSku, pristineItem, pristineItemList);
      } else {
        mappingVo = getItemAndBuildResponseByPristineItem(param, pristineItem, pristineItemList);
      }
      translateAttributeKeyName(mappingVo,
          productAttributesUtil.getCategoryListingParameterKey(pristineItem));
    }
    return mappingVo;
  }

  private void translateAttributeKeyName(PristineItemDetailAndMappingVo mappingVo, String[] categoryListingParametersMap) {
    mappingVo.setAttributes(productAttributesUtil.translatePristineListingAttributeName(
        mappingVo.getAttributes(), categoryListingParametersMap));
    mappingVo.getOtherPristineItems().stream().forEach(pristineSimilarItemVo ->
        pristineSimilarItemVo.setAttributes(
            productAttributesUtil.translatePristineListingAttributeName(
                pristineSimilarItemVo.getAttributes(), categoryListingParametersMap)));
  }

  private PristineItemDetailAndMappingVo getItemAndBuildResponseByPristineItem(
      MandatoryRequestParam param, PristineDataItem pristineItem, List<PristineDataItem> pristineItemList)
      throws Exception {
    PristineItemDetailAndMappingVo mappingVo;
    CacheItemVO cacheItemVO = pristineCacheableService.findItemByPristine(param.getStoreId(), pristineItem);
    if(cacheItemVO == null) {
      mappingVo = getMasterDataAndBuildResponse(param, pristineItem, pristineItemList,null);
    } else {
      mappingVo = getMasterDataAndBuildResponse(param, pristineItem, pristineItemList, cacheItemVO.getItemCode());
    }
    return mappingVo;
  }

  private PristineItemDetailAndMappingVo getItemAndBuildResponseByDefaultSku(
      MandatoryRequestParam param, String defaultSku, PristineDataItem pristineItem,
      List<PristineDataItem> pristineItemList) throws Exception {
    PristineItemDetailAndMappingVo mappingVo;
    Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        param.getStoreId(), defaultSku);
    if(Objects.isNull(item) || item.isMarkForDelete()) {
      mappingVo = getMasterDataAndBuildResponse(param, pristineItem, pristineItemList,null);
    } else {
      mappingVo = getMasterDataAndBuildResponse(param, pristineItem, pristineItemList, item.getItemCode());
    }
    return mappingVo;
  }

  private PristineItemDetailAndMappingVo getMasterDataAndBuildResponse(
      MandatoryRequestParam param, PristineDataItem pristineItem,
      List<PristineDataItem> pristineItemList, String itemCode) throws Exception {
    if(StringUtils.isBlank(itemCode)) {
      return buildResponseModel(itemCode, null, pristineItem, pristineItemList);
    } else {
      Set<String> itemSet = new HashSet<>();
      itemSet.add(itemCode);
      Map<String, MasterDataItem> masterDataItems = masterDataService
          .getMasterDataItems(param.getStoreId(), param.getUsername(), param.getRequestId(),
              itemSet);
      return buildResponseModel(itemCode, masterDataItems, pristineItem, pristineItemList);
    }
  }

  private PristineItemDetailAndMappingVo buildResponseModel(String itemCode,
      Map<String, MasterDataItem> masterDataItems, PristineDataItem pristineCollection,
      List<PristineDataItem> pristineItemList) throws Exception{
    String imageUrl = StringUtils.EMPTY;
    if(masterDataItems != null) {
      MasterDataItem item = masterDataItems.get(itemCode);
      imageUrl = item.getMasterDataItemImages().stream().filter(img -> img.isMainImage())
          .map(img -> img.getLocationPath()).findFirst().orElse(StringUtils.EMPTY);
    }

    if (StringUtils.isBlank(pristineCollection.getPristineBrand()) && StringUtils
        .isBlank(pristineCollection.getPristineModel())) {
      Item item = pristineCacheableService.findFirstItemByPristine(pristineCollection);
      if (Objects.nonNull(item)) {
        item = masterDataConstructorService.constructPristineDataItemWithMasterData(item);
        pristineCollection = item.getPristineDataItem();
        pristineCollection.setPristineListingAttributes(new HashMap<>());
      }
    }
    PristineItemDetailAndMappingVo mappingVo = new PristineItemDetailAndMappingVo();
    mappingVo.setId(pristineCollection.getPristineId());
    mappingVo.setName(pristineCollection.getPristineProductName());
    mappingVo.setBrand(pristineCollection.getPristineBrand());
    mappingVo.setAttributes(pristineCollection.getPristineListingAttributes());
    mappingVo.setImageUrl(imageUrl);
    List<PristineSimilarItemVo> similarItemVos = new ArrayList<>();
    for (PristineDataItem pristineItem : pristineItemList) {
      if (StringUtils.isBlank(pristineItem.getPristineBrand()) && StringUtils
          .isBlank(pristineItem.getPristineModel())) {
        Item item = pristineCacheableService.findFirstItemByPristine(pristineItem);
        if (Objects.nonNull(item)) {
          item = masterDataConstructorService.constructPristineDataItemWithMasterData(item);
          pristineItem = item.getPristineDataItem();
          pristineItem.setPristineListingAttributes(new HashMap<>());
        } else {
          continue;
        }
      }
      similarItemVos.add(new PristineSimilarItemVo(pristineItem.getPristineId(), pristineItem.getPristineProductName(),
          pristineItem.getPristineListingAttributes()));
    }
    mappingVo.setOtherPristineItems(similarItemVos);
    return mappingVo;
  }

  private String getPristineMasterIdByPristineId(String pristineId) {
    PristineDataItem pristineDataItem = this.pristineItemRepository.findByPristineId(pristineId);
    if (pristineDataItem != null) {
      return pristineDataItem.getPristineMasterId();
    }
    return StringUtils.EMPTY;
  }

  private String getPristineMasterIdByProductSku(String storeId, String productSku) {
    List<Item> items = cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    return items.stream().map(Item::getPristineDataItem).filter(Objects::nonNull)
        .map(PristineDataItem::getPristineMasterId).findFirst().orElse(StringUtils.EMPTY);
  }

  private String getPristineMasterIdByProductCode(String storeId, String productCode) {
    List<Product> products = productCacheableService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    List<String> productSkuList = products.stream().map(Product::getProductSku)
        .filter(StringUtils::isNotBlank).collect(Collectors.toList());
    return productSkuList.stream().map(productSku ->
        getPristineMasterIdByProductSku(storeId, productSku))
        .filter(StringUtils::isNotBlank).findFirst().orElse(StringUtils.EMPTY);
  }

  @Override
  public DefaultItemSkuVO getDefaultItemSkuByPristineId(MandatoryRequestParam param,
                                                        String pristineId) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(param.getStoreId()), STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pristineId), PRISTINE_ID_MUST_NOT_BE_BLANK);
    DefaultItemSkuVO defaultItemSkuVO = new DefaultItemSkuVO();
    defaultItemSkuVO.setDefaultItemSku(getDefaultItemSku(param.getStoreId(), param.getUsername(),
        param.getRequestId(), pristineId));
    return defaultItemSkuVO;
  }

  private String getDefaultItemSku(String storeId, String username,
                                   String requestId, String pristineId) {
    List<Item> itemList =
        itemCacheableService.findItemsByPristineId(storeId, username, requestId, pristineId, false)
            .stream().sorted(Comparator.comparing(item -> item.getPrice().stream()
            .filter(itemPrice -> itemPrice.getChannel().equals(DEFAULT_PRICE_CHANNEL))
            .findFirst().orElse(new Price()).getOfferPrice()))
            .collect(Collectors.toList());
    return (CollectionUtils.isNotEmpty(itemList)) ?
        itemList.get(0).getItemSku() : StringUtils.EMPTY;
  }

  @Override
  public String getPristineMasterIdByPristineIdOrProductCodeOrSku(
      String storeId, String username, String requestId, String pristineIdOrProductCodeOrSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pristineIdOrProductCodeOrSku),
        PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU_MUST_NOT_BE_BLANK);
    if (skuValidator.isPristineId(pristineIdOrProductCodeOrSku)) {
      return getPristineMasterIdByPristineId(pristineIdOrProductCodeOrSku);
    } else if (skuValidator.isProductSku(pristineIdOrProductCodeOrSku)) {
      return getPristineMasterIdByProductSku(storeId, pristineIdOrProductCodeOrSku);
    } else if (skuValidator.isProductCode(pristineIdOrProductCodeOrSku)) {
      return getPristineMasterIdByProductCode(storeId, pristineIdOrProductCodeOrSku);
    }
    return StringUtils.EMPTY;
  }

}
