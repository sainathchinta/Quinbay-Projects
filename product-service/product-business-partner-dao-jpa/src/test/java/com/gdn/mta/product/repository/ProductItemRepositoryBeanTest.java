package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.verification.VerificationMode;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public class ProductItemRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String PRODUCT_NAME="product-name";
  private static final String UPC_CODE="upc-code";
  private static final String FINAL_CATEGORY_ID="final-category-id";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final Pageable DEFAULT_PAGEABLE =
      PageRequest.of(0, 10, Sort.by(Direction.ASC, "id"));
  private static final String DEFAULT_CATEGORY_ID = "category-id";
  private String requestId;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private ProductItemRepositoryBean productItemRepositoryBean;

  @AfterEach
  public void finalizeTest() throws Exception {
   // Mockito.verifyNoMoreInteractions(this.productClient);
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    initMocks(this);
    this.requestId = UUID.randomUUID().toString();
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    GdnRestListResponse<ProductItemResponse> baseProductItemResponses =
        new GdnRestListResponse<ProductItemResponse>(productItemResponses,
            new PageMetaData(1, 0, 1), this.requestId);
    Mockito.when(
        this.pcbFeign.getProductItemByUpcCodeExactMatch(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt(), Mockito.anyInt(), Mockito.anyString())).thenReturn(baseProductItemResponses);
  }

  @Test
  public void testFindByStoreIdAndKeywordAndViewable() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    ProductItem productItem = this.getProductItem();
    productItem.setViewable(true);
    productItems.add(productItem);
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    for (ProductItem productItem2 : productItems) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      Image imageResponse = new Image();
      BeanUtils.copyProperties(productItem2, productItemResponse);
      BeanUtils.copyProperties(productItem2.getProductItemImages().get(0), imageResponse);
      productItemResponse.setImages(new ArrayList<Image>());
      productItemResponse.getImages().add(imageResponse);
      productItemResponses.add(productItemResponse);
    }
    GdnRestListResponse<ProductItemResponse> response =
        new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
            new PageMetaData(ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE.getPageSize(),
                ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE.getPageNumber(),
                productItemResponses.size()),
            this.requestId);
    Mockito.when(
        this.pcbFeign.getProductItemByViewableAndProductItemNameOrUpcCode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq("Item 1"), Mockito.eq(true),
            Mockito.eq(true))).thenReturn(response);
    Page<ProductItem> result = this.productItemRepositoryBean.findByStoreIdAndKeywordAndViewable(
        ProductItemRepositoryBeanTest.DEFAULT_STORE_ID, "Item 1", true, true,
        ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.pcbFeign, ProductItemRepositoryBeanTest.AT_LEAST_ONE)
        .getProductItemByViewableAndProductItemNameOrUpcCode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.eq("Item 1"), Mockito.eq(true),
            Mockito.eq(true));
    Assertions.assertEquals(1, result.getTotalElements());
  }

  @Test public void testfindByStoreIdAndNameOrUpcCode() throws Exception {
    final List<ProductCodeResponse> productCodeResponseList = new ArrayList<>();
    productCodeResponseList.add(new ProductCodeResponse("product_code", "product_name"));
    final List<AttributeReqModel> attributeReqModelList = new ArrayList<>();
    attributeReqModelList.add(new AttributeReqModel("name", "value"));
    final GdnRestListResponse<ProductCodeResponse> response =
        new GdnRestListResponse<ProductCodeResponse>(null, null, true, productCodeResponseList,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productCodeResponseList.size()), this.requestId);
    Mockito.when(pcbFeign.getProductItemLikeNameOrUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt(), Mockito.anyInt(), eq(UPC_CODE), eq(PRODUCT_NAME), eq(attributeReqModelList),
            eq(FINAL_CATEGORY_ID)))
        .thenReturn(response);
    productItemRepositoryBean
        .findByStoreIdAndNameOrUpcCode(DEFAULT_STORE_ID, PRODUCT_NAME, UPC_CODE, FINAL_CATEGORY_ID,
            attributeReqModelList, DEFAULT_PAGEABLE);
    Mockito.verify(this.pcbFeign, ProductItemRepositoryBeanTest.AT_LEAST_ONE)
        .getProductItemLikeNameOrUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt(), Mockito.anyInt(), eq(UPC_CODE), eq(PRODUCT_NAME), eq(attributeReqModelList),
            eq(FINAL_CATEGORY_ID));
  }

  @Test
  public void testFindByStoreIdAndUpcCode() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.getProductItem());
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    for (ProductItem productItem : productItems) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productItemResponses.add(productItemResponse);
    }
    GdnRestListResponse<ProductItemResponse> response =
        new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
            new PageMetaData(ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE.getPageSize(),
                ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE.getPageNumber(),
                productItemResponses.size()),
            this.requestId);
    Mockito.when(this.pcbFeign.getProductItemByUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.anyInt(), eq("1000000000001"))).thenReturn(response);
    Page<ProductItem> result =
        this.productItemRepositoryBean.findByStoreIdAndUpcCode(ProductItemRepositoryBeanTest.DEFAULT_STORE_ID,
            "1000000000001", ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.pcbFeign, ProductItemRepositoryBeanTest.AT_LEAST_ONE)
        .getProductItemByUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
            Mockito.anyInt(), eq("1000000000001"));
    Assertions.assertEquals(1, result.getTotalElements());
  }

  @Test
  public void findByStoreIdAndUpcCodeExactMatchTest() throws Exception {
    GdnRestListResponse<ProductItemResponse> response = new GdnRestListResponse<>(null, null, true, "");
    Mockito.when(
        pcbFeign.getProductItemByUpcCodeExactMatch(eq(DEFAULT_STORE_ID), Mockito.anyString(), Mockito.anyString(),
            eq(0), eq(10), eq(null))).thenReturn(response);
    this.productItemRepositoryBean.findByStoreIdAndUpcCodeExactMatch(
        ProductItemRepositoryBeanTest.DEFAULT_STORE_ID, null,
        ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.pcbFeign)
        .getProductItemByUpcCodeExactMatch(eq(DEFAULT_STORE_ID), Mockito.anyString(), Mockito.anyString(), eq(0),
            eq(10), eq(null));
  }

  @Test
  public void findByStoreIdAndProductItemNameAndCategoryIdTest() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.getProductItem());
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    for (ProductItem productItem : productItems) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productItemResponse.setImages(new ArrayList<Image>());
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemResponse.getImages().add(image);
      }
      productItemResponses.add(productItemResponse);
    }
    GdnRestListResponse<ProductItemResponse> response =
        new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
            new PageMetaData(ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE.getPageSize(),
                ProductItemRepositoryBeanTest.DEFAULT_PAGEABLE.getPageNumber(),
                productItemResponses.size()),
            this.requestId);
    Mockito.when(this.pcbFeign.getProductItemByProductItemNameAndCategoryId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    this.productItemRepositoryBean.findByStoreIdAndProductItemNameAndCategoryId(DEFAULT_STORE_ID, PRODUCT_NAME,
        DEFAULT_CATEGORY_ID, DEFAULT_PAGEABLE);
    Mockito.verify(this.pcbFeign)
        .getProductItemByProductItemNameAndCategoryId(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyString(),
            Mockito.anyString());
  }
  
  private ProductItem getProductItem() {
    Product product =
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build();
    product.setId(UUID.randomUUID().toString());
    ProductItem productItem =
        new ProductItem(product, "1000000000001", UUID.randomUUID().toString(), "Produk 1 Item 1",
            null, ProductItemRepositoryBeanTest.DEFAULT_STORE_ID);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    productItem.setProductItemImages(new ArrayList<ProductItemImage>());
    productItem.getProductItemImages().add(productItemImage);
    return productItem;
  }


}
