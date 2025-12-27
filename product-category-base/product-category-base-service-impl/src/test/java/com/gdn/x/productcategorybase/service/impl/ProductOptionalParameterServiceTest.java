package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.entity.ProductOptionalParameter;
import com.gdn.x.productcategorybase.repository.ProductOptionalParameterRepository;

public class ProductOptionalParameterServiceTest {
  private static final String PRODUCT_CODE = "GDN-01";
  private static final String NAME = "name";
  private static final boolean UNIQUE_FALSE = false;
  private static final String STORE_ID = "10001";
  private static final Integer DEFAULT_PAGE = 0;
  private static final Integer DEFAULT_SIZE = 10;
  private static final String ID = GdnUUIDHelper.generateUUID();

  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";

  @InjectMocks
  ProductOptionalParameterServiceBean service;

  @Mock
  ProductOptionalParameterRepository repository;

  private ProductOptionalParameter productOptionalParameter;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    this.productOptionalParameter = new ProductOptionalParameter(ProductOptionalParameterServiceTest.PRODUCT_CODE,
        ProductOptionalParameterServiceTest.NAME, ProductOptionalParameterServiceTest.UNIQUE_FALSE,
        ProductOptionalParameterServiceTest.STORE_ID);
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.repository);
  }

  @Test
  public void testFindById() throws Exception {
    this.productOptionalParameter.setId(ProductOptionalParameterServiceTest.ID);
    when(this.repository.findById(ProductOptionalParameterServiceTest.ID)).thenReturn(Optional.of(this.productOptionalParameter));
    this.service.findById(ProductOptionalParameterServiceTest.ID);
    verify(this.repository, times(1)).findById(ProductOptionalParameterServiceTest.ID);
  }

  @Test
  public void testFindByNameLikeWithList() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    when(this.repository
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(ProductOptionalParameterServiceTest.STORE_ID, "nam"))
            .thenReturn(productOptionalParameters);
    this.service.findByNameLike(ProductOptionalParameterServiceTest.STORE_ID, "nam");
    verify(this.repository, times(1))
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(ProductOptionalParameterServiceTest.STORE_ID, "nam");
  }

  @Test
  public void testFindByNameLikeWithPage() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    Page<ProductOptionalParameter> page = new PageImpl<ProductOptionalParameter>(productOptionalParameters);
    Pageable pageable = PageRequest.of(ProductOptionalParameterServiceTest.DEFAULT_PAGE,
        ProductOptionalParameterServiceTest.DEFAULT_SIZE);
    Mockito.when(this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "nam", pageable)).thenReturn(page);
    this.service.findByNameLike(ProductOptionalParameterServiceTest.STORE_ID, "nam", pageable);
    verify(this.repository, times(1)).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "nam", pageable);
  }

  @Test
  public void testFindByProductCodeLikeAndUniqueFalseWithList() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    when(this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN")).thenReturn(productOptionalParameters);
    this.service.findByProductCodeLikeAndUniqueFalse(ProductOptionalParameterServiceTest.STORE_ID, "GDN");
    Mockito.verify(this.repository, times(1))
        .findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(
            ProductOptionalParameterServiceTest.STORE_ID, "GDN");
  }

  @Test
  public void testFindByProductCodeLikeAndUniqueTrueWithList() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    when(this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN")).thenReturn(productOptionalParameters);
    this.service.findByProductCodeLikeAndUniqueTrue(ProductOptionalParameterServiceTest.STORE_ID, "GDN");
    verify(this.repository, times(1)).findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN");
  }

  @Test
  public void testFindByProductCodeLikeWithList() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    when(this.repository.findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN")).thenReturn(productOptionalParameters);
    this.service.findByProductCodeLike(ProductOptionalParameterServiceTest.STORE_ID, "GDN");
    verify(this.repository, times(1)).findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN");
  }

  @Test
  public void testFindByProductLikeAndUniqueFalseWithPage() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    Page<ProductOptionalParameter> page = new PageImpl<ProductOptionalParameter>(productOptionalParameters);
    Pageable pageable = PageRequest.of(ProductOptionalParameterServiceTest.DEFAULT_PAGE,
        ProductOptionalParameterServiceTest.DEFAULT_SIZE);
    when(this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable)).thenReturn(page);
    this.service.findByProductCodeLikeAndUniqueFalse(ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable);
    verify(this.repository, times(1)).findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable);
  }

  @Test
  public void testFindByProductLikeAndUniqueTrueWithPage() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    Page<ProductOptionalParameter> page = new PageImpl<ProductOptionalParameter>(productOptionalParameters);
    Pageable pageable = PageRequest.of(ProductOptionalParameterServiceTest.DEFAULT_PAGE,
        ProductOptionalParameterServiceTest.DEFAULT_SIZE);
    when(this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable)).thenReturn(page);
    this.service.findByProductCodeLikeAndUniqueTrue(ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable);
    verify(this.repository, times(1)).findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable);
  }

  @Test
  public void testFindByProductLikeWithPage() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    Page<ProductOptionalParameter> page = new PageImpl<ProductOptionalParameter>(productOptionalParameters);
    Pageable pageable = PageRequest.of(ProductOptionalParameterServiceTest.DEFAULT_PAGE,
        ProductOptionalParameterServiceTest.DEFAULT_SIZE);
    when(this.repository.findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable)).thenReturn(page);
    this.service.findByProductCodeLike(ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable);
    verify(this.repository, times(1)).findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(
        ProductOptionalParameterServiceTest.STORE_ID, "GDN", pageable);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    List<ProductOptionalParameter> productOptionalParameters = new ArrayList<ProductOptionalParameter>();
    productOptionalParameters.add(this.productOptionalParameter);
    Page<ProductOptionalParameter> page = new PageImpl<ProductOptionalParameter>(productOptionalParameters);
    Pageable pageable = PageRequest.of(ProductOptionalParameterServiceTest.DEFAULT_PAGE,
        ProductOptionalParameterServiceTest.DEFAULT_SIZE);
    when(this.repository.findByStoreIdAndMarkForDeleteFalse(ProductOptionalParameterServiceTest.STORE_ID, pageable))
        .thenReturn(page);
    this.service.findByStoreId(ProductOptionalParameterServiceTest.STORE_ID, pageable);
    verify(this.repository, times(1)).findByStoreIdAndMarkForDeleteFalse(ProductOptionalParameterServiceTest.STORE_ID,
        pageable);
  }

  @Test
  public void testMarkForDeleteProductOptionalParameter() throws Exception {
    this.productOptionalParameter.setId(ProductOptionalParameterServiceTest.ID);
    when(this.repository.findById(ProductOptionalParameterServiceTest.ID)).thenReturn(Optional.of(this.productOptionalParameter));
    this.service.markForDeleteProductOptionalParameter(ProductOptionalParameterServiceTest.ID);
    assertTrue(this.productOptionalParameter.isMarkForDelete());
    verify(this.repository, times(1)).findById(ProductOptionalParameterServiceTest.ID);
    verify(this.repository, times(1)).save(this.productOptionalParameter);
  }

  @Test
  public void testMarkForDeleteProductOptionalParameterFail() throws Exception {
    this.productOptionalParameter.setId(ProductOptionalParameterServiceTest.ID);
    when(this.repository.findById(ProductOptionalParameterServiceTest.ID)).thenReturn(Optional.of(this.productOptionalParameter));
    doThrow(RuntimeException.class).when(this.repository).save(this.productOptionalParameter);
    try {
      this.service.markForDeleteProductOptionalParameter(ProductOptionalParameterServiceTest.ID);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      ApplicationException exception = (ApplicationException) e;
      assertEquals(ErrorCategory.DATA_ACCESS, exception.getErrorCodes());
      verify(this.repository, times(1)).findById(ProductOptionalParameterServiceTest.ID);
      verify(this.repository, times(1)).save(this.productOptionalParameter);
    }
  }

  @Test
  public void testMarkForDeleteProductOptionalParameterIfIdNotExist() throws Exception {
    when(this.repository.findById(ProductOptionalParameterServiceTest.ID)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.markForDeleteProductOptionalParameter(ProductOptionalParameterServiceTest.ID);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      ApplicationException exception = (ApplicationException) e;
      assertEquals(ErrorCategory.DATA_NOT_FOUND, exception.getErrorCodes());
      verify(this.repository, times(1)).findById(ProductOptionalParameterServiceTest.ID);
    }
  }

  @Test
  public void testSaveProductOptionalParameterSuccessfully() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    ProductOptionalParameter newProductOptionalParameter = new ProductOptionalParameter();
    BeanUtils.copyProperties(this.productOptionalParameter, newProductOptionalParameter);
    newProductOptionalParameter.setId(id);
    when(this.repository.saveAndFlush(this.productOptionalParameter)).thenReturn(newProductOptionalParameter);

    assertEquals(this.service.save(this.productOptionalParameter), id);

    verify(this.repository, times(1)).saveAndFlush(this.productOptionalParameter);
  }

  @Test
  public void testSaveProductOptionalParameterWithFilledId() throws Exception {
    this.productOptionalParameter.setId(ProductOptionalParameterServiceTest.ID);
    when(this.repository.findById(ProductOptionalParameterServiceTest.ID)).thenReturn(Optional.of(
        this.productOptionalParameter));
    try {
      this.service.save(this.productOptionalParameter);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      assertTrue(e.getMessage().contains(ProductOptionalParameterServiceTest.ERROR_MESSAGE_FOR_SAVE));
      verify(this.repository, times(1)).findById(ProductOptionalParameterServiceTest.ID);
    }
  }

  @Test
  public void testUpdateProductOptionalParameterSuccessfully() throws Exception {
    this.productOptionalParameter.setId(ProductOptionalParameterServiceTest.ID);
    when(this.repository.findById(ProductOptionalParameterServiceTest.ID)).thenReturn(Optional.of(this.productOptionalParameter));
    when(this.repository.saveAndFlush(this.productOptionalParameter)).thenReturn(this.productOptionalParameter);

    this.service.update(this.productOptionalParameter);

    verify(this.repository, times(1)).findById(ProductOptionalParameterServiceTest.ID);
    verify(this.repository, times(1)).saveAndFlush(this.productOptionalParameter);
  }
  
  @Test
  public void testDelete() throws Exception {
    this.service.delete(ID);
  }
  
}
