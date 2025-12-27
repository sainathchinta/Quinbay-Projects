package com.gdn.partners.pdt.service.product;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;

import com.gdn.x.mta.distributiontask.dao.api.ProductAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;

public class ProductServiceTest {

  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductItemRepository productItemRepository;

  @Mock
  private ProductAttributeRepository productAttributeRepository;

  @Mock
  private ProductImageRepository productImageRepository;

  @Mock
  private ProductUtils productUtils;

  @InjectMocks
  private ProductServiceBean productServiceBean;

  private ProductImage generateProductImage() throws Exception {
    ProductImage productImage = new ProductImage();
    return productImage;
  }

  private List<ProductImage> generateProductImages() throws Exception {
    List<ProductImage> productImages = new ArrayList<ProductImage>();
    productImages.add(this.generateProductImage());
    return productImages;
  }

  private ProductAttribute generateProductAttribute() throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    return productAttribute;
  }

  private List<ProductAttribute> generateProductAttributes() throws Exception {
    List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    productAttributes.add(this.generateProductAttribute());
    return productAttributes;
  }

  private ProductItem generateProductItem() throws Exception {
    ProductItem productItem = new ProductItem();
    return productItem;
  }

  private List<ProductItem> generateProductItems() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.generateProductItem());
    return productItems;
  }

  private Product generateProduct() throws Exception {
    Product product = new Product();
    product.setProductItems(this.generateProductItems());
    product.setProductAttributes(this.generateProductAttributes());
    product.setProductImages(this.generateProductImages());
    return product;
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.doNothing().when(this.productItemRepository).deleteAll(Mockito.anyList());
    Mockito.doNothing().when(this.productAttributeRepository).deleteAll(Mockito.anyList());
    Mockito.doNothing().when(this.productImageRepository).deleteAll(Mockito.anyList());
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(null);
    Mockito.when(this.productUtils.regenerateProductReplacementDetails(Mockito.any(Product.class),
        Mockito.any(Product.class))).thenReturn(generateProduct());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productItemRepository);
    Mockito.verifyNoMoreInteractions(this.productAttributeRepository);
    Mockito.verifyNoMoreInteractions(this.productImageRepository);
    Mockito.verifyNoMoreInteractions(this.productUtils);
  }

  @SuppressWarnings("unchecked")
  @Test
   void overwriteTest() throws Exception {
    this.productServiceBean.overwrite(this.generateProduct(), this.generateProduct());
    Mockito.verify(this.productItemRepository).deleteAll(Mockito.anyList());
    Mockito.verify(this.productAttributeRepository).deleteAll(Mockito.anyList());
    Mockito.verify(this.productImageRepository).deleteAll(Mockito.anyList());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productUtils).regenerateProductReplacementDetails(
        Mockito.any(Product.class), Mockito.any(Product.class));
  }

  @SuppressWarnings("unchecked")
  @Test
   void overwriteWithEmptyProductItemsAndEmptyProductAttributesAndEmptyProductImagesTest()
      throws Exception {
    Product savedProduct = this.generateProduct();
    savedProduct.getProductItems().clear();
    savedProduct.getProductAttributes().clear();
    savedProduct.getProductImages().clear();
    this.productServiceBean.overwrite(savedProduct, this.generateProduct());
    Mockito.verify(this.productItemRepository, ProductServiceTest.NEVER_CALLED)
        .deleteAll(Mockito.anyList());
    Mockito.verify(this.productAttributeRepository, ProductServiceTest.NEVER_CALLED)
        .deleteAll(Mockito.anyList());
    Mockito.verify(this.productImageRepository, ProductServiceTest.NEVER_CALLED)
        .deleteAll(Mockito.anyList());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productUtils).regenerateProductReplacementDetails(
        Mockito.any(Product.class), Mockito.any(Product.class));
  }
}
