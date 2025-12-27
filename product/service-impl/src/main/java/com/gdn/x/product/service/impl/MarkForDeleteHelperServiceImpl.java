package com.gdn.x.product.service.impl;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;

import com.gdn.x.product.service.api.MarkForDeleteHelperService;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

@Service
public class MarkForDeleteHelperServiceImpl implements MarkForDeleteHelperService {

  @Override
  public ProductDetailResponse removeAllMarkForDelete(ProductDetailResponse product) {
    product.setImages(removeMarkForDeleteImages(product.getImages()));
    product.setProductCategoryResponses(removeMarkForDelete(product.getProductCategoryResponses()));
    product
        .setProductAttributeResponses(removeMarkForDelete(product.getProductAttributeResponses()));
    for (ProductAttributeResponse productAttribute : product.getProductAttributeResponses()) {
      productAttribute.setProductAttributeValues(removeMarkForDelete(productAttribute
          .getProductAttributeValues()));
    }
    product.setProductItemResponses(removeMarkForDelete(product.getProductItemResponses()));
    for (ProductItemResponse itemResponse : product.getProductItemResponses()) {
      itemResponse.setImages(removeMarkForDeleteImages(itemResponse.getImages()));
      itemResponse.setProductItemAttributeValueResponses(removeMarkForDelete(itemResponse
          .getProductItemAttributeValueResponses()));
    }
    return product;
  }

  @Override
  public ProductItemDetailResponse removeAllMarkForDelete(ProductItemDetailResponse productItem) {
    productItem.setImages(removeMarkForDeleteImages(productItem.getImages()));
    productItem.setProductItemAttributeValueResponses(removeMarkForDelete(productItem
        .getProductItemAttributeValueResponses()));
    productItem.getProductResponse().setImages(
        removeMarkForDeleteImages(productItem.getProductResponse().getImages()));
    return productItem;
  }

  private <T extends BaseDTOResponse> List<T> removeMarkForDelete(List<T> list) {
    if (list != null) {
      for (int i = list.size() - 1; i >= 0; i--) {
        if (list.get(i).isMarkForDelete()) {
          list.remove(i);
        }
      }
    }
    return list;
  }

  private <T extends BaseDTOResponse> Set<T> removeMarkForDelete(Set<T> set) {
    if (set != null) {
      for (Iterator<T> i = set.iterator(); i.hasNext();) {
        T element = i.next();
        if (element.isMarkForDelete()) {
          i.remove();
        }
      }
    }
    return set;
  }

  private List<Image> removeMarkForDeleteImages(List<Image> list) {
    if (list != null) {
      for (int i = list.size() - 1; i >= 0; i--) {
        if (list.get(i).isMarkForDelete()) {
          list.remove(i);
        }
      }
    }
    return list;
  }

}
