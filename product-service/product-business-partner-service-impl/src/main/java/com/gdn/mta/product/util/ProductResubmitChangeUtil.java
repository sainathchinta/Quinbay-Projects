package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3ItemWipRequest;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3WipRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProductResubmitChangeUtil {

  public static List<ProductFieldHistory> getProductDiff(ProductCreationRequest newProduct,
      ProductDetailResponse oldProduct) {

    List<ProductFieldHistory> changedFieldList = new ArrayList<>();

    getChangedFieldsByProductRequest(oldProduct, changedFieldList, newProduct);

    //check if editable fields of product items are same or not
    Map<String, ProductItemCreationRequest> productItemMap = new HashMap<>();
    for (ProductItemCreationRequest newItem : newProduct.getProductItemRequests()) {
      productItemMap.put(newItem.getProductItemId(), newItem);
    }

    for (ProductItemResponse oldItem : oldProduct.getProductItemResponses()) {
      ProductItemCreationRequest newItem = productItemMap.get(oldItem.getId());
      if (Objects.nonNull(newItem)) {
        if (!StringUtils.isAnyBlank(oldItem.getUpcCode(), newItem.getUpcCode()) && !StringUtils
            .equals(oldItem.getUpcCode(), newItem.getUpcCode())) {
          changedFieldList.add(new ProductFieldHistory(oldItem.getGeneratedItemName() + SaveHistoryConstants.UPC_CODE,
              oldItem.getUpcCode(), newItem.getUpcCode()));
        }
      }
    }
    // check if attribute value is changed or not. Currently checking only for SINGLE and
    // PREDEFINED values
    getChangedAttributes(oldProduct, changedFieldList, newProduct);
    return changedFieldList;
  }

  public static List<ProductFieldHistory> getProductDiff(ProductResubmitRequest newProduct,
      ProductDetailResponse oldProduct) {
    List<ProductFieldHistory> changedFieldList = new ArrayList<>();
    getChangedFieldsByProductRequest(oldProduct, changedFieldList, newProduct.getProductRequest());
    //check if editable fields of product items are same or not
    Map<String, ProductItemRequest> productItemMap = new HashMap<>();
    for (ProductItemRequest productItemRequest : newProduct.getProductRequest().getProductItems()) {
      if (Objects.nonNull(productItemRequest)) {
        productItemMap.put(productItemRequest.getId(), productItemRequest);
      }
    }

    for (ProductItemResponse oldItem : oldProduct.getProductItemResponses()) {
      ProductItemRequest newItem = productItemMap.get(oldItem.getId());
      if (Objects.nonNull(newItem)) {
        if (!StringUtils.isAnyBlank(oldItem.getUpcCode(), newItem.getUpcCode()) && !StringUtils
            .equals(oldItem.getUpcCode(), newItem.getUpcCode())) {
          changedFieldList.add(new ProductFieldHistory(oldItem.getGeneratedItemName() + SaveHistoryConstants.UPC_CODE,
              oldItem.getUpcCode(), newItem.getUpcCode()));
        }
      }
    }

    // check if attribute value is changed or not. Currently checking only for SINGLE and
    // PREDEFINED values
    getChangedAttributes(oldProduct, changedFieldList, newProduct.getProductRequest());
    return changedFieldList;
  }

  private static void getChangedAttributes(ProductDetailResponse oldProduct, List<ProductFieldHistory> changedFieldList,
      ProductRequest productRequest) {
    Map<String, Set<String>> newProductAttrValue = getProductAttrsAndValueMap(productRequest.getProductAttributes());
    Map<String, Set<String>> oldProductAttrValue = getProductAttrsAndValueMap(oldProduct);

    for (ProductAttributeResponse attr : oldProduct.getProductAttributeResponses()) {
      String id = attr.getId();
      Set<String> oldAttrPredefinedValueList =
          oldProductAttrValue.get(constructProductAttrKey(id, DescriptiveAttributeValueType.PREDEFINED));
      Set<String> oldAttrDescriptiveValueList =
          oldProductAttrValue.get(constructProductAttrKey(id, DescriptiveAttributeValueType.SINGLE));
      Set<String> newAttrPredefinedValueList =
          newProductAttrValue.get(constructProductAttrKey(id, DescriptiveAttributeValueType.PREDEFINED));
      Set<String> newAttrDescriptiveValueList =
          newProductAttrValue.get(constructProductAttrKey(id, DescriptiveAttributeValueType.SINGLE));
      if (CollectionUtils.isNotEmpty(oldAttrDescriptiveValueList) && CollectionUtils
          .isNotEmpty(newAttrDescriptiveValueList)) {
        if (!(CollectionUtils.isEqualCollection(oldAttrDescriptiveValueList, newAttrDescriptiveValueList)
            && CollectionUtils.isEqualCollection(oldAttrPredefinedValueList, newAttrPredefinedValueList))) {
          changedFieldList.add(
              new ProductFieldHistory(SaveHistoryConstants.ATTR_FIELD + " : " + attr.getProductAttributeName(),
                  CollectionUtils.union(oldAttrDescriptiveValueList, oldAttrPredefinedValueList),
                  CollectionUtils.union(newAttrDescriptiveValueList, newAttrPredefinedValueList)));
        }
      }
    }
  }

  private static void getChangedFieldsByProductRequest(ProductDetailResponse oldProduct,
      List<ProductFieldHistory> changedFieldList, ProductRequest productRequest) {
    if (!StringUtils.equals(oldProduct.getBrand(), productRequest.getBrand())) {
      changedFieldList.add(
          new ProductFieldHistory(SaveHistoryConstants.BRAND_FIELD, oldProduct.getBrand(), productRequest.getBrand()));
    }

    if (!StringUtils.equals(oldProduct.getName(), productRequest.getName())) {
      changedFieldList.add(
          new ProductFieldHistory(SaveHistoryConstants.NAME_FIELD, oldProduct.getName(), productRequest.getName()));
    }

    if (ArrayUtils.isEmpty(oldProduct.getDescription())) {
      oldProduct.setDescription(new byte[1]);
    }

    if (ArrayUtils.isEmpty(productRequest.getDescription())) {
      productRequest.setDescription(new byte[1]);
    }

    if (!new String(oldProduct.getDescription()).trim().equals(new String(productRequest.getDescription()).trim())) {
      changedFieldList.add(new ProductFieldHistory(SaveHistoryConstants.DESC_FIELD,
          getUIVisibleString(new String(oldProduct.getDescription()).trim()),
          getUIVisibleString(new String(productRequest.getDescription()).trim())));
    }

    if (!oldProduct.getLength().equals(productRequest.getLength())) {
      changedFieldList.add(new ProductFieldHistory(SaveHistoryConstants.LENGTH_FIELD, oldProduct.getLength(),
          productRequest.getLength()));
    }

    if (!oldProduct.getWidth().equals(productRequest.getWidth())) {
      changedFieldList.add(
          new ProductFieldHistory(SaveHistoryConstants.WIDTH_FIELD, oldProduct.getWidth(), productRequest.getWidth()));
    }

    if (!oldProduct.getWeight().equals(productRequest.getWeight())) {
      changedFieldList.add(new ProductFieldHistory(SaveHistoryConstants.WEIGHT_FIELD, oldProduct.getWeight(),
          productRequest.getWeight()));
    }

    if (!oldProduct.getHeight().equals(productRequest.getHeight())) {
      changedFieldList.add(new ProductFieldHistory(SaveHistoryConstants.HEIGHT_FIELD, oldProduct.getHeight(),
          productRequest.getHeight()));
    }

    if (!StringUtils.equals(oldProduct.getUniqueSellingPoint(), productRequest.getUniqueSellingPoint())) {
      changedFieldList.add(new ProductFieldHistory(SaveHistoryConstants.USP_FIELD,
          getUIVisibleString(oldProduct.getUniqueSellingPoint()),
          getUIVisibleString(productRequest.getUniqueSellingPoint())));
    }

    if (!StringUtils.equals(
        StringUtils.isNotEmpty(oldProduct.getProductStory()) ? oldProduct.getProductStory().trim() : StringUtils.EMPTY,
        StringUtils.isNotEmpty(productRequest.getProductStory()) ?
            productRequest.getProductStory().trim() :
            StringUtils.EMPTY)) {
      changedFieldList.add(new ProductFieldHistory(SaveHistoryConstants.PRODUCT_STORY_FIELD,
          getUIVisibleString(oldProduct.getProductStory()), getUIVisibleString(productRequest.getProductStory())));
    }
  }

  private static Map<String, Set<String>> getProductAttrsAndValueMap(List<ProductAttributeRequest> attributeRequests) {
    Map<String, Set<String>> productAttrValue = new HashMap<>();
    for (ProductAttributeRequest productAttr : attributeRequests) {
      Set<String> descriptiveValueList = new HashSet<>();
      Set<String> predefinedValueList = new HashSet<>();
      for (ProductAttributeValueRequest attrValue : productAttr.getProductAttributeValues()) {
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
    }
    return productAttrValue;
  }

  private static Map<String, Set<String>> getProductAttrsAndValueMap(ProductDetailResponse product) {
    Map<String, Set<String>> productAttrValue = new HashMap<>();
    for (ProductAttributeResponse productAttr : product.getProductAttributeResponses()) {
      Set<String> descriptiveValueList = new HashSet<>();
      Set<String> predefinedValueList = new HashSet<>();
      for (ProductAttributeValueResponse attrValue : productAttr.getProductAttributeValues()) {
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
    }
    return productAttrValue;
  }


  private static String constructProductAttrKey(String id, DescriptiveAttributeValueType type) {
    return id + "_" + type;
  }

  private static String getUIVisibleString(String str) {
    str = StringUtils.removeStart(str, SaveHistoryConstants.START_PARAGRAPH);
    str = StringUtils.removeEnd(str, SaveHistoryConstants.END_PARAGRAPH);
    return str;
  }

}
