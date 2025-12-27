package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

@Repository
public class ProductItemRepositoryBean implements ProductItemRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public Page<ProductItem> findByStoreIdAndKeywordAndViewable(String storeId, String keyword,
      boolean viewable, boolean isOnlyExternal, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductItemResponse> response =
        this.pcbFeign.getProductItemByViewableAndProductItemNameOrUpcCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), keyword, viewable, isOnlyExternal);
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    for (ProductItemResponse productItemResponse : response.getContent()) {
      ProductItem productItem = new ProductItem();
      productItem.setProductItemImages(new ArrayList<ProductItemImage>());
      BeanUtils.copyProperties(productItemResponse, productItem);

      if (!productItemResponse.getImages().isEmpty()) {
        ProductItemImage productItemImage = new ProductItemImage();
        BeanUtils.copyProperties(productItemResponse.getImages().get(0), productItemImage);
        productItem.getProductItemImages().add(productItemImage);
      }
      productItems.add(productItem);
    }
    Page<ProductItem> page = new PageImpl<ProductItem>(productItems, pageable,
        response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<ProductCodeResponse> findByStoreIdAndNameOrUpcCode(String storeId,
      String productName, String upcCode, String finalCategoryId,
      List<AttributeReqModel> modelList, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductCodeResponse> response =
        this.pcbFeign.getProductItemLikeNameOrUpcCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), upcCode, productName, modelList, finalCategoryId);
    final Page<ProductCodeResponse> page = new PageImpl<ProductCodeResponse>(response.getContent(),
        pageable, response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<ProductItem> findByStoreIdAndUpcCode(String storeId, String upcCode,
      Pageable pageable) throws Exception {
    GdnRestListResponse<ProductItemResponse> response =
        this.pcbFeign.getProductItemByUpcCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), upcCode);
    List<ProductItem> productItems = convertToProductResponses(response);
    Page<ProductItem> page = new PageImpl<ProductItem>(productItems, pageable,
        response.getPageMetaData().getTotalRecords());
    return page;
  }
  @Override
  public Page<ProductItem> findByStoreIdAndUpcCodeExactMatch(String storeId, String upcCode,
      Pageable pageable) throws Exception {
    GdnRestListResponse<ProductItemResponse> response =
        this.pcbFeign.getProductItemByUpcCodeExactMatch(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), upcCode);
    List<ProductItem> productItemList = convertToProductResponses(response);
    Page<ProductItem> page = new PageImpl<>(productItemList, pageable,
        response.getPageMetaData().getTotalRecords());
    return page;
  }

  private List<ProductItem> convertToProductResponses(GdnRestListResponse<ProductItemResponse> response) {
    List<ProductItem> productItemList = new ArrayList<ProductItem>();
    for (ProductItemResponse productItemResponse : response.getContent()) {
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(productItemResponse, productItem);
      productItemList.add(productItem);
    }
    return productItemList;
  }

  @Override
  public Page<ProductItem> findByStoreIdAndProductItemNameAndCategoryId(String storeId,
      String itemName, String categoryId, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductItemResponse> response =
        this.pcbFeign.getProductItemByProductItemNameAndCategoryId(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            pageable.getPageNumber(), pageable.getPageSize(), itemName, categoryId);
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    for (ProductItemResponse productItemResponse : response.getContent()) {
      ProductItem productItem = new ProductItem();
      productItem.setProductItemImages(new ArrayList<ProductItemImage>());
      BeanUtils.copyProperties(productItemResponse, productItem);

      if (!productItemResponse.getImages().isEmpty()) {
        ProductItemImage productItemImage = new ProductItemImage();
        BeanUtils.copyProperties(productItemResponse.getImages().get(0), productItemImage);
        productItem.getProductItemImages().add(productItemImage);
      }
      productItems.add(productItem);
    }
    Page<ProductItem> page =
        new PageImpl<ProductItem>(productItems, pageable, response.getPageMetaData()
            .getTotalRecords());
    return page;
  }

}
