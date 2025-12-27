package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAttributeDetailVo;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Utility class for product-related operations.
 * Contains static methods that were previously in service classes to avoid circular dependencies.
 */
public class ProductUtil {

    /**
     * Converts AddVariantRequestVo to Item entity.
     * 
     * @param addVariantRequestVo the variant request VO
     * @param product the product entity
     * @param mandatoryRequestParam the mandatory request parameters
     * @return the converted Item entity
     */
    public static Item convertToItem(AddVariantRequestVo addVariantRequestVo, Product product,
        MandatoryRequestParam mandatoryRequestParam) {
        Item item = new Item();
        BeanUtils.copyProperties(addVariantRequestVo, item, "definingAttributes");
        item.setProductSku(product.getProductSku());
        item.setMerchantCode(product.getMerchantCode());
        item.setSynchronized(product.isSynchronized());
        item.setItemCatentryId(addVariantRequestVo.getItemSku());
        item.setLateFulfillment(!ProductType.REGULAR.equals(product.getProductType()));
        item.setOff2OnChannelActive(product.isOff2OnChannelActive());
        item.setFreeSample(product.isFreeSample());
        item.setCategoryCode(product.getCategoryCode());
        item.setBrand(product.getBrand());
        item.setDefiningAttributes(addVariantRequestVo.getDefiningAttributes().stream()
            .map(ProductUtil::getProductAttributeDetail)
            .collect(Collectors.toList()));
        item.setStoreId(mandatoryRequestParam.getStoreId());
        item.setCreatedBy(mandatoryRequestParam.getUsername());
        item.setUpdatedBy(mandatoryRequestParam.getUsername());
        return item;
    }

    /**
     * Converts ProductAttributeDetailVo to ProductAttributeDetail entity.
     * 
     * @param productAttributeDetailVo the VO to convert
     * @return the converted entity
     */
    private static ProductAttributeDetail getProductAttributeDetail(ProductAttributeDetailVo productAttributeDetailVo) {
        ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
        BeanUtils.copyProperties(productAttributeDetailVo, productAttributeDetail);
        return productAttributeDetail;
    }

    /**
     * Gets buyable status by channel with schedule consideration.
     * 
     * @param itemViewConfigs the item view configurations
     * @param channel the channel
     * @param isArchived whether the item is archived
     * @return the buyable status
     * @throws Exception if no config found for channel
     */
    public static boolean getBuyableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
        throws Exception {
        ItemViewConfig foundConfig = itemViewConfigs.stream()
                .filter(config -> channel.equals(config.getChannel()))
                .findFirst().orElse(null);

        if (Objects.isNull(foundConfig)) {
            throw new Exception("No item view config available for channel " + channel);
        }

        if (isArchived) {
            return false;
        }

        ItemBuyableSchedule itemBuyableSchedule = foundConfig.getItemBuyableSchedules();
        if (Objects.isNull(itemBuyableSchedule)) {
            return foundConfig.isBuyable();
        }

        Date currDate = new Date();
        if (Optional.of(itemBuyableSchedule).map(ItemBuyableSchedule::getStartDateTime)
            .filter(currDate::after).isPresent() && Optional.of(itemBuyableSchedule)
            .map(ItemBuyableSchedule::getEndDateTime).filter(currDate::before).isPresent()) {
            return itemBuyableSchedule.isBuyable();
        }
        return foundConfig.isBuyable();
    }

    /**
     * Gets discoverable status by channel with schedule consideration.
     * 
     * @param itemViewConfigs the item view configurations
     * @param channel the channel
     * @param isArchived whether the item is archived
     * @return the discoverable status
     * @throws Exception if no config found for channel
     */
    public static boolean getDiscoverableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
        throws Exception {
        ItemViewConfig foundConfig = itemViewConfigs.stream()
                .filter(config -> channel.equals(config.getChannel()))
                .findFirst().orElse(null);

        if (Objects.isNull(foundConfig)) {
            throw new Exception("No item view config available for channel " + channel);
        }

        if (isArchived) {
            return false;
        }

        ItemDiscoverableSchedule itemDiscoverableSchedule = foundConfig.getItemDiscoverableSchedules();
        if (itemDiscoverableSchedule == null) {
            return foundConfig.isDiscoverable();
        }

        Date currDate = new Date();
        if (Objects.nonNull(itemDiscoverableSchedule.getStartDateTime()) && Objects.nonNull(
            itemDiscoverableSchedule.getEndDateTime())) {
            if (currDate.after(itemDiscoverableSchedule.getStartDateTime()) && currDate.before(
                itemDiscoverableSchedule.getEndDateTime())) {
                return itemDiscoverableSchedule.isDiscoverable();
            }
        }
        return foundConfig.isDiscoverable();
    }

    /**
     * Gets original buyable status by channel.
     * 
     * @param itemViewConfigs the item view configurations
     * @param channel the channel
     * @param isArchived whether the item is archived
     * @return the original buyable status
     * @throws Exception if no config found for channel
     */
    public static boolean getOriginalBuyableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel,
        boolean isArchived) throws Exception {
        Boolean buyable = itemViewConfigs.stream()
                .filter(config -> channel.equals(config.getChannel()))
                .map(ItemViewConfig::isBuyable)
                .findFirst().orElse(null);
        if (Objects.isNull(buyable)) {
            throw new Exception("No item view config available for channel " + channel);
        }
        if (isArchived) {
            return false;
        }
        return buyable;
    }

    /**
     * Gets original discoverable status by channel.
     * 
     * @param itemViewConfigs the item view configurations
     * @param channel the channel
     * @param isArchived whether the item is archived
     * @return the original discoverable status
     * @throws Exception if no config found for channel
     */
    public static boolean getOriginalDiscoverableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel,
        boolean isArchived) throws Exception {
        Boolean discoverable = itemViewConfigs.stream()
                .filter(config -> channel.equals(config.getChannel()))
                .map(ItemViewConfig::isDiscoverable)
                .findFirst().orElse(null);
        if (Objects.isNull(discoverable)) {
            throw new Exception("No item view config available for channel " + channel);
        }
        if (isArchived) {
            return false;
        }
        return discoverable;
    }

    /**
     * Gets price by channel.
     * 
     * @param prices the set of prices
     * @param channel the channel
     * @return the price for the channel
     * @throws Exception if no price found for channel
     */
    public static Price getPriceByChannel(Set<Price> prices, String channel) throws Exception {
        Price foundPrice = prices.stream()
                .filter(price -> channel.equals(price.getChannel()))
                .findFirst().orElse(null);
        if (foundPrice == null) {
            throw new Exception("No price available for channel " + channel);
        }
        return foundPrice;
    }

    /**
     * Converts offline items to a map grouped by item SKU.
     * 
     * @param offlineItemsMap the map to populate
     * @param offlineItems the list of offline items
     */
    public static void convertToOfflineItemMap(Map<String, List<OfflineItem>> offlineItemsMap, List<OfflineItem> offlineItems) {
        for (OfflineItem offlineItem : offlineItems) {
            String itemSku = offlineItem.getItemSku();
            offlineItemsMap.computeIfAbsent(itemSku, k -> new ArrayList<>());
            offlineItemsMap.get(itemSku).add(offlineItem);
        }
    }

    /**
     * Generates specification detail HTML from product attributes.
     * 
     * @param product the product entity
     * @return HTML specification detail string
     */
    public static String generateSpecificationDetail(Product product) {
        MasterDataProduct masterDataProduct = product.getMasterDataProduct();
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("<ul>");
        Map<String, List<String>> attributeCodeToValuesMap = new HashMap<String, List<String>>();

        if (!CollectionUtils.isEmpty(product.getDefiningAttributes())) {
            for (ProductAttribute defining : product.getDefiningAttributes()) {
                if (!CollectionUtils.isEmpty(defining.getProductAttributeDetails())) {
                    for (ProductAttributeDetail detail : defining.getProductAttributeDetails()) {
                        String attributeCode = detail.getAttributeCode();
                        if (attributeCodeToValuesMap.get(attributeCode) == null) {
                            attributeCodeToValuesMap.put(attributeCode, new ArrayList<String>());
                        }
                        attributeCodeToValuesMap.get(attributeCode).add(detail.getAttributeValue());
                    }
                }
            }
        }

        if (!CollectionUtils.isEmpty(masterDataProduct.getMasterDataProductAttributes())) {
            for (MasterDataProductAttribute productAttribute : masterDataProduct
                .getMasterDataProductAttributes()) {
                MasterDataAttribute attribute = productAttribute.getMasterDataAttribute();
                if (!attribute.isSkuValue()) {
                    List<MasterDataProductAttributeValue> productAttributeValues =
                        productAttribute.getMasterDataProductAttributeValues();
                    stringBuilder.append("<li>" + attribute.getAttributeName() + "<ul>");
                    if (MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
                        if (!MapUtils.isEmpty(attributeCodeToValuesMap)) {
                            for (String value : attributeCodeToValuesMap.get(attribute.getAttributeCode())) {
                                stringBuilder.append("<li>" + value + "</li>");
                            }
                        }
                    } else if (MasterDataAttributeType.PREDEFINED_ATTRIBUTE
                        .equals(attribute.getAttributeType())) {
                        for (MasterDataProductAttributeValue productAttributeValue : productAttributeValues) {
                            PredefinedAllowedAttributeValue predefined =
                                productAttributeValue.getPredefinedAllowedAttributeValue();
                            String value = null;
                            if (predefined != null) {
                                value = predefined.getValue();
                            }
                            stringBuilder.append("<li>" + value + "</li>");
                        }
                    } else if (MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE
                        .equals(attribute.getAttributeType())) {
                        for (MasterDataProductAttributeValue productAttributeValue : productAttributeValues) {
                            stringBuilder
                                .append("<li>" + productAttributeValue.getDescriptiveAttributeValue() + "</li>");
                        }
                    }
                    stringBuilder.append("</ul></li>");
                }
            }
        }
        stringBuilder.append("</ul>");
        return stringBuilder.toString();
    }

    /**
     * Constructs product with item and master data.
     * 
     * @param productAvailables the list of available products
     * @param itemAvailableCandidate the map of available items
     * @param masterDataProducts the map of master data products
     * @return map of product and items VO
     */
    public static Map<String, List<ProductAndItemsVO>> constructProductWithItemAndMasterData(
        List<Product> productAvailables, Map<String, List<Item>> itemAvailableCandidate,
        Map<String, MasterDataProductAndItemsVO> masterDataProducts) {
        Map<String, List<ProductAndItemsVO>> result = new HashMap<>();
        if(!CollectionUtils.isEmpty(productAvailables)){
            for (Product productCandidate : productAvailables) {
                try {
                    String key = StringUtils.EMPTY;
                    GdnPreconditions.checkArgument(itemAvailableCandidate != null,
                        "Available item candidate is null");
                    List<Item> items = itemAvailableCandidate.get(productCandidate.getProductSku());

                    if (productCandidate.isSynchronized()) {
                        key = productCandidate.getProductCode();
                        GdnPreconditions.checkArgument(masterDataProducts != null,
                            "Master data product is null");
                        MasterDataProductAndItemsVO masterDataProductAndItems = masterDataProducts.get(key);
                        MasterDataProduct masterDataProduct = masterDataProductAndItems.getMasterDataProduct();
                        MasterDataProduct simplifiedMasterDataProduct = new MasterDataProduct();
                        simplifiedMasterDataProduct.setShippingWeight(masterDataProduct.getShippingWeight());
                        productCandidate.setMasterDataProduct(simplifiedMasterDataProduct);

                        if(!CollectionUtils.isEmpty(items)){
                            for (Item item : items) {
                                MasterDataItem masterDataItem =
                                    masterDataProductAndItems.getMasterDataItems().get(item.getItemCode());
                                MasterDataItem simplifiedMasterDataItem = new MasterDataItem();
                                simplifiedMasterDataItem.setItemDeliveryWeight(masterDataItem.getItemDeliveryWeight());
                                item.setMasterDataItem(simplifiedMasterDataItem);
                            }
                        }
                    }
                    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(productCandidate, items);
                    List<ProductAndItemsVO> productAndItemsVOs = result.get(key);
                    if (productAndItemsVOs == null) {
                        productAndItemsVOs = new ArrayList<>();
                        result.put(key, productAndItemsVOs);
                    }
                    productAndItemsVOs.add(productAndItemsVO);
                } catch (Exception e) {
                    // Log error but continue processing
                }
            }
        }
        return result;
    }

    /**
     * Gets the settlement type based on product type and item properties.
     * 
     * @param product the product entity
     * @param item the item entity
     * @return the settlement type string
     */
    public static String getSettlementType(Product product, Item item) {
        if (ProductType.REGULAR.equals(product.getProductType())
            || ProductType.BIG_PRODUCT.equals(product.getProductType())) {
            return "REGULAR";
        } else if (StringUtils.isNotBlank(item.getTicketTemplateCode())) {
            return "BARCODE";
        } else {
            return "SETTLEMENT_CODE";
        }
    }

    /**
     * Fetches category ID from product detail response.
     * 
     * @param productDetailResponse the product detail response
     * @param productSku the product SKU
     * @param throwExceptionOnMissingCategoryId whether to throw exception on missing category ID
     * @return the category ID
     */
    public static String fetchCategoryIdFromProductDetailResponse(
        ProductDetailResponse productDetailResponse, String productSku, boolean throwExceptionOnMissingCategoryId) {
        if (throwExceptionOnMissingCategoryId) {
            return Optional.ofNullable(productDetailResponse)
                .map(ProductDetailResponse::getProductCategoryResponses).stream().flatMap(List::stream)
                .findFirst().map(ProductCategoryResponse::getCategory).map(CategoryResponse::getId)
                .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                    String.format(ErrorMessages.CATEGORY_ID_NOT_FOUND_IN_PCB_RESPONSE, productSku)));
        }
        return productDetailResponse.getProductCategoryResponses().get(0).getCategory().getId();
    }

    public static Product deleteItemAttributeFromProductAttribute(Product product, String itemSku) {
        if (product == null) {
            return null;
        }
        for (int i = 0; i < product.getDefiningAttributes().size(); i++) {
            if (itemSku.equals(product.getDefiningAttributes().get(i).getItemSku())) {
                product.getDefiningAttributes().remove(i);
                break;
            }
        }
        return product;
    }

    public static void overwriteItemPriceWithOfflinePrice(Set<Price> prices, OfflineItem offlineItem) {
        if (!CollectionUtils.isEmpty(prices) && offlineItem != null) {
            for (Price price : prices) {
                if (price != null) {
                    price.setListPrice(
                        Optional.ofNullable(offlineItem.getListPrice()).orElse(offlineItem.getOfferPrice()));
                    price.setOfferPrice(offlineItem.getOfferPrice());
                    price.setLastUpdatedBy(offlineItem.getUpdatedBy());
                    price.setLastUpdatedDate(offlineItem.getUpdatedDate());
                }
            }
        }
    }

    public static void overwriteItemViewConfigsForOfflineItem(Set<ItemViewConfig> itemViewConfigs) {
        if (!CollectionUtils.isEmpty(itemViewConfigs)) {
            for (ItemViewConfig itemViewConfig : itemViewConfigs) {
                if (itemViewConfig != null) {
                    itemViewConfig.setBuyable(Boolean.TRUE);
                    itemViewConfig.setDiscoverable(Boolean.TRUE);
                    itemViewConfig.setItemBuyableSchedules(null);
                    itemViewConfig.setItemDiscoverableSchedules(null);
                }
            }
        }
    }
}
