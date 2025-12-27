package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

/**
 * Created by Kesha on 11/04/16.
 */
@Component
public class ProductChangeUtil {

  protected static final String BRAND_FIELD = "Brand";
  protected static final String WIDTH_FIELD = "Lebar";
  protected static final String LENGTH_FIELD = "Panjang";
  protected static final String DESC_FIELD = "Deskripsi";
  protected static final String NAME_FIELD = "Nama Produk";
  protected static final String WEIGHT_FIELD = "Berat";
  protected static final String HEIGHT_FIELD = "Tinggi";
  protected static final String PRODUCT_STORY_FIELD = "Produk Story";
  protected static final String USP_FIELD = "Unique Selling Point";
  protected static final String URL_VIDEO = "Url video";
  protected static final String ATTR_FIELD = "Atribut";
  protected static final String GOODS_LEVEL = "Dangerous Good Level";
  protected static final String UPC_CODE = "Kode Manufaktur";
  protected static final String FAMILY_COLOUR = "Family Colour";
  public static final String CATEGORY = "Category";
  private static final String PROMO_SKU = "Promo Sku";
  private static final String START_PARAGRAPH = "<p>";
  private static final String END_PARAGRAPH = "</p>";
  private static final String VAT_APPLICABLE = "Vat Applicable";
  private static final String ITEM_IMAGES_ADDED = " Item images added ";
  private static final String ITEM_IMAGES_DELETED = " Item images deleted ";
  private static final Set<String> DIMENSION_FIELDS =
      ImmutableSet.of(LENGTH_FIELD, WIDTH_FIELD, HEIGHT_FIELD, WEIGHT_FIELD);


  public List<ProductFieldHistory> getProductDiff(Product newProduct, Product oldProduct) {

    List<ProductFieldHistory> changedFieldList = new ArrayList<>();

    if (!StringUtils.equals(oldProduct.getBrand(), newProduct.getBrand())) {
      changedFieldList.add(
          new ProductFieldHistory(BRAND_FIELD, oldProduct.getBrand(), newProduct.getBrand()));

    }
    if (!StringUtils.equals(oldProduct.getName(), newProduct.getName())) {
      changedFieldList
          .add(new ProductFieldHistory(NAME_FIELD, oldProduct.getName(), newProduct.getName()));

    }
    if(oldProduct.getDescription() == null){
      oldProduct.setDescription(new byte[1]);
    }
    if(newProduct.getDescription() == null){
      newProduct.setDescription(new byte[1]);
    }
    if (!new String(oldProduct.getDescription()).trim().equals(
        new String(newProduct.getDescription()).trim())) {
      changedFieldList.add(new ProductFieldHistory(DESC_FIELD, getUIVisibleString(new String(
          oldProduct.getDescription()).trim()), getUIVisibleString(new String(newProduct
          .getDescription()).trim())));
    }
    if (!oldProduct.getLength().equals(newProduct.getLength())) {
      changedFieldList.add(
          new ProductFieldHistory(LENGTH_FIELD, oldProduct.getLength(), newProduct.getLength()));

    }
    if (!oldProduct.getWidth().equals(newProduct.getWidth())) {
      changedFieldList.add(
          new ProductFieldHistory(WIDTH_FIELD, oldProduct.getWidth(), newProduct.getWidth()));

    }
    if (!oldProduct.getWeight().equals(newProduct.getWeight())) {
      changedFieldList.add(
          new ProductFieldHistory(WEIGHT_FIELD, oldProduct.getWeight(), newProduct.getWeight()));

    }
    if (!oldProduct.getHeight().equals(newProduct.getHeight())) {
      changedFieldList.add(
          new ProductFieldHistory(HEIGHT_FIELD, oldProduct.getHeight(), newProduct.getHeight()));

    }
    if(newProduct.isPromoSKU() != oldProduct.isPromoSKU()) {
      changedFieldList.add(
          new ProductFieldHistory(PROMO_SKU, oldProduct.isPromoSKU(), newProduct.isPromoSKU()));
    }

    if (!StringUtils
        .equals(oldProduct.getUniqueSellingPoint(), newProduct.getUniqueSellingPoint())) {
      changedFieldList.add(new ProductFieldHistory(USP_FIELD, getUIVisibleString(oldProduct
          .getUniqueSellingPoint()),
          getUIVisibleString(newProduct.getUniqueSellingPoint())));

    }

    if (!StringUtils
        .equals(oldProduct.getUrl(), newProduct.getUrl())) {
      changedFieldList.add(new ProductFieldHistory(URL_VIDEO, getUIVisibleString(oldProduct
          .getUrl()),
          getUIVisibleString(newProduct.getUrl())));

    }

    if (!StringUtils.equals(oldProduct.getProductStory() != null ? oldProduct.getProductStory()
        .trim() : new String(), newProduct.getProductStory() != null ? newProduct.getProductStory()
        .trim() : new String())) {
      changedFieldList.add(new ProductFieldHistory(PRODUCT_STORY_FIELD,
          getUIVisibleString(oldProduct.getProductStory()), getUIVisibleString(newProduct
              .getProductStory())));

    }
    //check if editable fields of product items are same or not
    Map<String, ProductItem> productItemMap = new HashMap<>();
    for (ProductItem newItem : newProduct.getProductItems()) {
      productItemMap.put(newItem.getId(), newItem);
    }

    for (ProductItem oldItem : oldProduct.getProductItems()) {
      ProductItem newItem = productItemMap.get(oldItem.getId());

      if (newItem != null) {
        if (oldItem.getDangerousGoodsLevel() != null && newItem.getDangerousGoodsLevel() != null
            && !oldItem.getDangerousGoodsLevel().equals(newItem
            .getDangerousGoodsLevel())) {
          changedFieldList.add(new ProductFieldHistory(
              oldItem.getGeneratedItemName() + " : " + GOODS_LEVEL,
              oldItem.getDangerousGoodsLevel(), newItem.getDangerousGoodsLevel()));

        }
        if (!StringUtils.equals(oldItem.getUpcCode(), newItem.getUpcCode())) {
          changedFieldList.add(new ProductFieldHistory(oldItem.getGeneratedItemName() + UPC_CODE, oldItem.getUpcCode(),
              newItem.getUpcCode()));
        }

        if (CollectionUtils.isNotEmpty(oldItem.getProductItemAttributeValues()) && CollectionUtils
            .isNotEmpty(newItem.getProductItemAttributeValues())) {
          List<ProductItemAttributeValue> productItemAttributeValues = oldItem.getProductItemAttributeValues().stream()
              .filter(
                  productItemAttributeValue -> FAMILY_COLOUR.equals(productItemAttributeValue.getAttribute().getName()))
              .collect(Collectors.toList());
          List<ProductItemAttributeValue> newProductItemAttributeValues =
              newItem.getProductItemAttributeValues().stream().filter(
                  productItemAttributeValue -> FAMILY_COLOUR.equals(productItemAttributeValue.getAttribute().getName()))
                  .collect(Collectors.toList());
          String newItemFamilyColour = null;
          String oldItemFamilyColour = null;
          if (CollectionUtils.isNotEmpty(productItemAttributeValues) && CollectionUtils
              .isNotEmpty(newProductItemAttributeValues)) {
            oldItemFamilyColour = productItemAttributeValues.get(0).getValue();
            newItemFamilyColour = newProductItemAttributeValues.get(0).getValue();
          }
          oldItemFamilyColour = Objects.isNull(oldItemFamilyColour) ? StringUtils.EMPTY : oldItemFamilyColour;
          if (StringUtils.isNotEmpty(newItemFamilyColour) && !oldItemFamilyColour.equals(newItemFamilyColour)) {
            changedFieldList.add(
                new ProductFieldHistory(oldItem.getGeneratedItemName() + StringUtils.SPACE + FAMILY_COLOUR,
                    oldItemFamilyColour, newItemFamilyColour));
          }
        }

        if (!oldProduct.isReviewPending()) {
          if (CollectionUtils.isNotEmpty(oldItem.getProductItemImages()) && CollectionUtils
              .isNotEmpty(newItem.getProductItemImages())) {
            List<ProductItemImage> oldMainImage =
                oldItem.getProductItemImages().stream().filter(productItemImage -> productItemImage.isMainImages())
                    .filter(productItemImage -> !productItemImage.isMarkForDelete()).collect(Collectors.toList());
            List<ProductItemImage> newMainImage =
                newItem.getProductItemImages().stream().filter(productItemImage -> productItemImage.isMainImages())
                    .filter(productItemImage -> !productItemImage.isMarkForDelete()).collect(Collectors.toList());

            List<String> oldItemImageLocationPaths =
                oldItem.getProductItemImages().stream().filter(Predicate.not(ProductItemImage::isMarkForDelete))
                    .map(ProductItemImage::getLocationPath).collect(Collectors.toList());
            List<String> newItemImageLocationPaths =
                newItem.getProductItemImages().stream().filter(Predicate.not(ProductItemImage::isMarkForDelete))
                    .map(ProductItemImage::getLocationPath).collect(Collectors.toList());
            newItem.getProductItemImages().stream().filter(Predicate.not(ProductItemImage::isMarkForDelete))
                .filter(productItemImage -> !oldItemImageLocationPaths.contains(productItemImage.getLocationPath()))
                .forEach(productItemImage -> changedFieldList.add(
                    new ProductFieldHistory(oldItem.getGeneratedItemName().concat(ITEM_IMAGES_ADDED), null,
                        productItemImage.getLocationPath())));
            oldItemImageLocationPaths.removeAll(newItemImageLocationPaths);
            oldItemImageLocationPaths.forEach(locationPath -> changedFieldList.add(
                new ProductFieldHistory(oldItem.getGeneratedItemName().concat(ITEM_IMAGES_DELETED), locationPath, null)));

            if (CollectionUtils.isNotEmpty(oldMainImage) && CollectionUtils.isNotEmpty(newMainImage) && !oldMainImage
                .get(0).getLocationPath().equals(newMainImage.get(0).getLocationPath())) {
              changedFieldList
                  .add(new ProductFieldHistory(oldItem.getGeneratedItemName() + " Item images edited", null, null));
            }
          }
        }
        if (!Objects.equals(newItem.getVatApplicable(), oldItem.getVatApplicable())) {
          changedFieldList.add(new ProductFieldHistory(oldItem.getGeneratedItemName() + StringUtils.SPACE + VAT_APPLICABLE,
            oldItem.getVatApplicable(), newItem.getVatApplicable()));
        }
      }
    }

    Category oldCategory = Iterables.getLast(oldProduct.getProductCategories()).getCategory();
    Category newCategory = Iterables.getLast(newProduct.getProductCategories()).getCategory();

    if (Objects.nonNull(oldCategory) && Objects.nonNull(newCategory) && !oldCategory.getName()
        .equals(newCategory.getName())) {
      changedFieldList.add(new ProductFieldHistory(CATEGORY, oldCategory.getName(), newCategory.getName()));
    }

    // check if attribute value is changed or not. Currently checking only for SINGLE and
    // PREDEFINED values
    Map<String, Set<String>> newProductAttrValue = getProductAttrsAndValueMap(newProduct, changedFieldList);
    Map<String, Set<String>> oldProductAttrValue = getProductAttrsAndValueMap(oldProduct, changedFieldList);

    for (ProductAttribute attr : oldProduct.getProductAttributes()) {
      String id = attr.getId();
      Set<String> oldAttrPredefinedValueList = oldProductAttrValue.get(constructProductAttrKey
          (id, DescriptiveAttributeValueType.PREDEFINED));
      Set<String> oldAttrDescriptiveValueList = oldProductAttrValue.get(constructProductAttrKey
          (id, DescriptiveAttributeValueType.SINGLE));
      Set<String> newAttrPredefinedValueList = newProductAttrValue.get(constructProductAttrKey
          (id, DescriptiveAttributeValueType.PREDEFINED));
      Set<String> newAttrDescriptiveValueList = newProductAttrValue.get(constructProductAttrKey
          (id, DescriptiveAttributeValueType.SINGLE));
      if ((CollectionUtils.isNotEmpty(oldAttrDescriptiveValueList) && CollectionUtils
          .isNotEmpty(newAttrDescriptiveValueList)) || (CollectionUtils.isNotEmpty(oldAttrPredefinedValueList)
          && CollectionUtils.isNotEmpty(newAttrPredefinedValueList))) {
        if (!(CollectionUtils.isEqualCollection(oldAttrDescriptiveValueList, newAttrDescriptiveValueList)
            && CollectionUtils.isEqualCollection(oldAttrPredefinedValueList, newAttrPredefinedValueList))) {
          if (!StringUtils.equals(BRAND_FIELD, attr.getAttribute().getName()) || changedFieldList.stream()
              .map(ProductFieldHistory::getFieldName)
              .noneMatch(fieldName -> StringUtils.equals(BRAND_FIELD, fieldName))) {
            changedFieldList.add(new ProductFieldHistory(ATTR_FIELD + " : " + attr.getAttribute().getName(),
                CollectionUtils.union(oldAttrDescriptiveValueList, oldAttrPredefinedValueList),
                CollectionUtils.union(newAttrDescriptiveValueList, newAttrPredefinedValueList)));
          }
        }
      } else {
        if (CollectionUtils.isNotEmpty(attr.getProductAttributeValues())) {
          if ((DescriptiveAttributeValueType.SINGLE
              .equals(attr.getProductAttributeValues().get(0).getDescriptiveAttributeValueType()))
              || (DescriptiveAttributeValueType.PREDEFINED
              .equals(attr.getProductAttributeValues().get(0).getDescriptiveAttributeValueType()))) {
            changedFieldList.add(new ProductFieldHistory(ATTR_FIELD + " : " + attr.getProductAttributeName(),
                CollectionUtils.union(oldAttrDescriptiveValueList, oldAttrPredefinedValueList),
                "This attribute was deleted after category change"));
          }
        }
      }
    }
    return changedFieldList;
  }

  public List<ProductFieldHistory> getChangedProductFields(Product product,
    SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO) {

    List<ProductFieldHistory> changedFieldList = new ArrayList<>();
    if(!product.getBrand().equals(simpleMasterProductUpdateRequestDTO.getBrand())){
      changedFieldList.add(
          new ProductFieldHistory(BRAND_FIELD, product.getBrand(), simpleMasterProductUpdateRequestDTO.getBrand()));
    }
    if(!product.getName().equals(simpleMasterProductUpdateRequestDTO.getName())){
      changedFieldList.add(
          new ProductFieldHistory(NAME_FIELD, product.getName(), simpleMasterProductUpdateRequestDTO.getName()));
    }
    if(!product.getHeight().equals(simpleMasterProductUpdateRequestDTO.getHeight())){
      changedFieldList.add(
          new ProductFieldHistory(HEIGHT_FIELD, product.getHeight(), simpleMasterProductUpdateRequestDTO.getHeight()));
    }
    if(!product.getLength().equals(simpleMasterProductUpdateRequestDTO.getLength())){
      changedFieldList.add(
          new ProductFieldHistory(LENGTH_FIELD, product.getLength(), simpleMasterProductUpdateRequestDTO.getLength()));
    }
    if(!product.getWidth().equals(simpleMasterProductUpdateRequestDTO.getWidth())){
      changedFieldList.add(
          new ProductFieldHistory(WIDTH_FIELD, product.getWidth(), simpleMasterProductUpdateRequestDTO.getWidth()));
    }
    if(!product.getWeight().equals(simpleMasterProductUpdateRequestDTO.getWeight())){
      changedFieldList.add(
          new ProductFieldHistory(WEIGHT_FIELD, product.getWeight(), simpleMasterProductUpdateRequestDTO.getWeight()));
    }
    product.getProductItems().stream()
        .filter(item -> Objects.nonNull(item.getDangerousGoodsLevel()))
        .filter(item -> !item.getDangerousGoodsLevel().equals(simpleMasterProductUpdateRequestDTO.getDangerousGoodsLevel()))
        .forEach(item -> changedFieldList.add(new ProductFieldHistory(
            item.getGeneratedItemName() + " : " + GOODS_LEVEL, item.getDangerousGoodsLevel(),
            simpleMasterProductUpdateRequestDTO.getDangerousGoodsLevel())));
    return changedFieldList;
  }

  private Map<String, Set<String>> getProductAttrsAndValueMap(Product product,
      List<ProductFieldHistory> changedFieldList) {
    Map<String, Set<String>> productAttrValue = new HashMap<>();
    for (ProductAttribute productAttr : product.getProductAttributes()) {
      if (StringUtils.isNotBlank(productAttr.getId())) {
        Set<String> descriptiveValueList = new HashSet<>();
        Set<String> predefinedValueList = new HashSet<>();
        for (ProductAttributeValue attrValue : productAttr.getProductAttributeValues()) {
          if (attrValue.getDescriptiveAttributeValueType().equals(DescriptiveAttributeValueType.SINGLE)) {
            descriptiveValueList.add(getUIVisibleString(attrValue.getDescriptiveAttributeValue()));
          } else if (attrValue.getDescriptiveAttributeValueType().equals(DescriptiveAttributeValueType.PREDEFINED)) {
            predefinedValueList.add(getUIVisibleString(attrValue.getPredefinedAllowedAttributeValue().getValue()));
          }
        }
        //constructing key from productId and valueType to be unique and easy to get
        productAttrValue.put(constructProductAttrKey(productAttr.getId(), DescriptiveAttributeValueType.SINGLE),
            descriptiveValueList);
        productAttrValue.put(constructProductAttrKey(productAttr.getId(), DescriptiveAttributeValueType.PREDEFINED),
            predefinedValueList);
      } else {
        if (CollectionUtils.isNotEmpty(productAttr.getProductAttributeValues())) {
          if ((DescriptiveAttributeValueType.SINGLE
              .equals(productAttr.getProductAttributeValues().get(0).getDescriptiveAttributeValueType()))
              || (DescriptiveAttributeValueType.PREDEFINED
              .equals(productAttr.getProductAttributeValues().get(0).getDescriptiveAttributeValueType()))) {
            changedFieldList.add(new ProductFieldHistory(ATTR_FIELD + " : " + productAttr.getProductAttributeName(),
                "This is newly added attribute", productAttr.getProductAttributeName()));
          }
        }
      }
    }
    return productAttrValue;
  }

  private String constructProductAttrKey(String id, DescriptiveAttributeValueType type) {
    return id + "_" + type;
  }

  private String getUIVisibleString(String str) {
    str = StringUtils.removeStart(str, START_PARAGRAPH);
    str = StringUtils.removeEnd(str, END_PARAGRAPH);
    return str;
  }

  public Set<String> getCategoryIds(Product newProduct, Product oldProduct) {
    Set<String> categoryIds = new HashSet<>();
    categoryIds.add(Iterables.getLast(oldProduct.getProductCategories()).getCategory().getId());
    categoryIds.add(Iterables.getLast(newProduct.getProductCategories()).getCategory().getId());
    return categoryIds;
  }

  public boolean isDimensionFieldsChanged(List<ProductFieldHistory> changedFieldList) {
    for (ProductFieldHistory productFieldHistory : changedFieldList) {
      if (DIMENSION_FIELDS.contains(productFieldHistory.getFieldName()) || productFieldHistory.getFieldName()
          .contains(GOODS_LEVEL)) {
        return true;
      }
    }
    return false;
  }

  public static boolean stockChanged(Integer deltaStock) {
    return Objects.nonNull(deltaStock) && deltaStock != 0;
  }
}
