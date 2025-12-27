package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.service.api.ProductService;

public class DeleteOriginalImagesForProductAndItemsEventListenerTest {
  private ObjectMapper mapper;
  private Product product;
  private static final String FILE_NAME_PATH1 = "1/CODE2/Testing1.txt";

  @InjectMocks
  DeleteOriginalImagesForProductAndItemsEventListener deleteOriginalImagesForProductAndItemsEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productService);
  }


  @Test
  void deleteOriginalImagesForProductAndItemsTest() throws Exception {
    Product product = new Product();
    List<ProductImage> productImageList = new ArrayList<ProductImage>();
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setLocationPath(FILE_NAME_PATH1);
    productImage.setMainImage(true);
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setProductItem(productItem);
    productItem.getProductItemImages().add(productItemImage);
    product.getProductItems().add(productItem);
    productImageList.add(productImage);
    product.setProductImages(productImageList);
    String message = mapper.writeValueAsString(product);
    Mockito.when(objectMapper.readValue(message, Product.class)).thenReturn(product);
    deleteOriginalImagesForProductAndItemsEventListener.deleteOriginalImages(message);
    Mockito.verify(objectMapper).readValue(message, Product.class);
    Mockito.verify(productService).deleteOriginalImagesForProductAndItems(product);
  }

  @Test
   void deleteOriginalImagesForProductAndItemsExceptionTest() throws Exception {
    Product product = new Product();
    List<ProductImage> productImageList = new ArrayList<ProductImage>();
    product.setProductImages(productImageList);
    String message = mapper.writeValueAsString(product);
    Mockito.when(objectMapper.readValue(message,Product.class)).thenThrow(new NullPointerException());
    try {
      deleteOriginalImagesForProductAndItemsEventListener.deleteOriginalImages(message);
    } finally {
      Mockito.verify(objectMapper).readValue(message, Product.class);
    }
  }
}
