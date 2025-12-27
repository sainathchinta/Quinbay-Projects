package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.entity.AutoApprovedProductsUserFeedback;
import com.gdn.partners.product.analytics.model.enums.AutoApprovedActionsEnum;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import com.gdn.partners.product.analytics.repository.UserFeedbackRepository;
import com.gdn.partners.product.analytics.service.impl.exception.ValidationException;
import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import com.gdn.partners.product.analytics.web.model.ProductAssigneeChangeResponse;
import com.gdn.partners.product.analytics.web.model.SellerResponse;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import org.springframework.test.util.ReflectionTestUtils;

public class AutoApprovedServiceImplTest {

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String PRODUCT_CODE = "productCode";
  private static final String USERNAME = "username";
  private static final String PRODUCT_CODE_1 = "productCode1";
  private static final String PRODUCT_CODE_2 = "productCode2";
  private static final String PRODUCT_CODE_3 = "productCode3";

  @InjectMocks
  private AutoApprovedServiceImpl autoApprovedService;

  @Mock
  private AutoApprovedRepository autoApprovedRepository;

  @Mock
  private UserFeedbackRepository userFeedbackRepository;

  @Captor
  private ArgumentCaptor<AutoApprovedProducts> autoApprovedProductsArgumentCaptor;

  private AutoApprovedAssigneeRequest assigneeRequest;
  private AutoApprovedProducts autoApprovedProducts;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    assigneeRequest = new AutoApprovedAssigneeRequest();
    autoApprovedProducts = new AutoApprovedProducts();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(autoApprovedRepository);
    Mockito.verifyNoMoreInteractions(userFeedbackRepository);
  }


  @Test
  public void testFetchListOfAutoApprovedProducts() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setKeyword("Test");
    AutoApprovedProducts product = new AutoApprovedProducts();
    Page<AutoApprovedProducts> autoApprovedProductsList =
      new PageImpl<>(Collections.singletonList(product));
    Mockito.when(
        autoApprovedRepository.fetchListOfAutoApprovedProducts(request, PageRequest.of(PAGE, SIZE),
          false))
      .thenReturn(autoApprovedProductsList);
    Page<AutoApprovedListWebResponse> result =
      autoApprovedService.fetchListOfAutoApprovedProducts(request, PAGE, SIZE);
    Assertions.assertEquals(1, result.getContent().size());
    Mockito.verify(autoApprovedRepository)
      .fetchListOfAutoApprovedProducts(request, PageRequest.of(PAGE, SIZE), false);
  }

  @Test
  public void testFetchListOfAutoApprovedProductsNull() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    Mockito.when(
        autoApprovedRepository.fetchListOfAutoApprovedProducts(request, PageRequest.of(PAGE, SIZE),
          false))
      .thenReturn(null);
    Page<AutoApprovedListWebResponse> result =
      autoApprovedService.fetchListOfAutoApprovedProducts(request, PAGE, SIZE);
    Mockito.verify(autoApprovedRepository)
      .fetchListOfAutoApprovedProducts(request, PageRequest.of(PAGE, SIZE), false);
  }

  @Test
  public void testFetchListOfAutoApprovedProductsEmpty() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    Page<AutoApprovedProducts> autoApprovedProductsList = new PageImpl<>(Collections.EMPTY_LIST);
    Mockito.when(
        autoApprovedRepository.fetchListOfAutoApprovedProducts(request, PageRequest.of(PAGE, SIZE),
          false))
      .thenReturn(autoApprovedProductsList);
    Page<AutoApprovedListWebResponse> result =
      autoApprovedService.fetchListOfAutoApprovedProducts(request, PAGE, SIZE);
    Assertions.assertEquals(0, result.getContent().size());
    Mockito.verify(autoApprovedRepository)
      .fetchListOfAutoApprovedProducts(request, PageRequest.of(PAGE, SIZE), false);
  }

  @Test
  public void updateAssigneeTest() {
    assigneeRequest.setProductCode(Collections.singletonList(PRODUCT_CODE));
    assigneeRequest.setAssigneeTo(USERNAME);
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(autoApprovedProducts);
    List<ProductAssigneeChangeResponse> responseList = new ArrayList<>();
    List<ProductAssigneeChangeResponse> result =
      autoApprovedService.updateAssignee(assigneeRequest);
    Assertions.assertEquals(responseList, result);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(autoApprovedRepository).save(autoApprovedProductsArgumentCaptor.capture());
    Assertions.assertEquals(USERNAME, autoApprovedProductsArgumentCaptor.getValue().getAssignedTo());
  }

  @Test
  public void unAssigneeTest() {
    assigneeRequest.setProductCode(Collections.singletonList(PRODUCT_CODE));
    assigneeRequest.setAssigneeTo(StringUtils.EMPTY);
    autoApprovedProducts.setAssignedTo(USERNAME);
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(autoApprovedProducts);
    List<ProductAssigneeChangeResponse> responseList = new ArrayList<>();
    List<ProductAssigneeChangeResponse> result =
      autoApprovedService.updateAssignee(assigneeRequest);
    Assertions.assertEquals(responseList, result);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(autoApprovedRepository).save(autoApprovedProductsArgumentCaptor.capture());
    Assertions.assertEquals(StringUtils.EMPTY,
      autoApprovedProductsArgumentCaptor.getValue().getAssignedTo());
  }

  @Test
  public void updateAssigneeEmptyProductCodeTest() {
    assigneeRequest.setAssigneeTo(USERNAME);
    try {
      autoApprovedService.updateAssignee(assigneeRequest);
    } catch (Exception e) {
      Assertions.assertEquals(ErrorCode.PRODUCT_CODE_LIST_EMPTY.getMessage(), e.getMessage());
      Assertions.assertEquals(ValidationException.class, e.getClass());
    }
  }

  @Test
  public void updateAssigneeProductNotFoundTest() {
    assigneeRequest.setProductCode(Collections.singletonList(PRODUCT_CODE));
    assigneeRequest.setAssigneeTo(USERNAME);
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(null);
    List<ProductAssigneeChangeResponse> responseList = Collections.singletonList(
      new ProductAssigneeChangeResponse(PRODUCT_CODE, ErrorCode.PRODUCT_NOT_FOUND.getMessage()));
    List<ProductAssigneeChangeResponse> result =
      autoApprovedService.updateAssignee(assigneeRequest);
    Assertions.assertEquals(responseList, result);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
  }

  @Test
  public void updateAssigneeProductEmptyTest() {
    assigneeRequest.setProductCode(Collections.singletonList(StringUtils.EMPTY));
    assigneeRequest.setAssigneeTo(USERNAME);
    List<ProductAssigneeChangeResponse> responseList = Collections.singletonList(
      new ProductAssigneeChangeResponse(StringUtils.EMPTY,
        ErrorCode.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage()));
    List<ProductAssigneeChangeResponse> result =
      autoApprovedService.updateAssignee(assigneeRequest);
    Assertions.assertEquals(responseList, result);
  }

  @Test
  public void updateAssigneeProductAlreadyUnAssignedTest() {
    assigneeRequest.setProductCode(Collections.singletonList(PRODUCT_CODE));
    assigneeRequest.setAssigneeTo(StringUtils.EMPTY);
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(autoApprovedProducts);
    List<ProductAssigneeChangeResponse> responseList = Collections.singletonList(
      new ProductAssigneeChangeResponse(PRODUCT_CODE,
        ErrorCode.PRODUCT_IS_ALREADY_UNASSIGNED.getMessage()));
    List<ProductAssigneeChangeResponse> result =
      autoApprovedService.updateAssignee(assigneeRequest);
    Assertions.assertEquals(responseList, result);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
  }

  @Test
  public void deleteAutoApprovedProductAutoHealTest() {
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(new AutoApprovedProducts());
    autoApprovedService.deleteAutoApprovedProduct(PRODUCT_CODE,
      AutoApprovedActionsEnum.AUTO_HEAL.name());
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(autoApprovedRepository).save(Mockito.any(AutoApprovedProducts.class));
  }

  @Test
  public void deleteAutoApprovedProductAutoHealTest_noAutoApprovedProductsPresent() {
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(null);
    autoApprovedService.deleteAutoApprovedProduct(PRODUCT_CODE,
        AutoApprovedActionsEnum.AUTO_HEAL.name());
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
  }

  @Test
  public void deleteAutoApprovedProductAcceptFeedbackNullTest() {
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(new AutoApprovedProducts());
    AutoApprovedProductsUserFeedback feedback = new AutoApprovedProductsUserFeedback();
    feedback.setProductCode(PRODUCT_CODE);
    feedback.setAction(AutoApprovedActionsEnum.ACCEPT.name());
    autoApprovedService.deleteAutoApprovedProduct(PRODUCT_CODE,
      AutoApprovedActionsEnum.ACCEPT.name());
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(userFeedbackRepository).save(feedback);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(autoApprovedRepository).save(Mockito.any(AutoApprovedProducts.class));
  }

  @Test
  public void deleteAutoApprovedProductAcceptFeedbackPresentTest() {
    AutoApprovedProductsUserFeedback feedback = new AutoApprovedProductsUserFeedback();
    feedback.setProductCode(PRODUCT_CODE);
    feedback.setAction(AutoApprovedActionsEnum.ACCEPT.name());
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(new AutoApprovedProducts());
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(feedback);
    autoApprovedService.deleteAutoApprovedProduct(PRODUCT_CODE,
      AutoApprovedActionsEnum.ACCEPT.name());
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(autoApprovedRepository).save(Mockito.any(AutoApprovedProducts.class));
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(userFeedbackRepository).save(feedback);
  }

  @Test
  public void testFetchSelectedItemsOfAutoApprovedProducts_Success() {
    AutoApprovedSelectedDownloadRequest request = new AutoApprovedSelectedDownloadRequest();
    request.setProductCodes(Arrays.asList(PRODUCT_CODE_1, PRODUCT_CODE_2, PRODUCT_CODE_3));
    AutoApprovedProducts product1 = new AutoApprovedProducts();
    AutoApprovedProducts product2 = new AutoApprovedProducts();
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE_1))
      .thenReturn(product1);
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE_2))
      .thenReturn(null);
    Mockito.when(autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE_3))
      .thenReturn(product2);
    AutoApprovedListWebResponse response1 = new AutoApprovedListWebResponse();
    response1.setSeller(new SellerResponse());
    AutoApprovedListWebResponse response2 = new AutoApprovedListWebResponse();
    response2.setSeller(new SellerResponse());
    List<AutoApprovedListWebResponse> responseList = new ArrayList<>(List.of(response1, response2));
    List<AutoApprovedListWebResponse> result =
      autoApprovedService.fetchSelectedItemsOfAutoApprovedProducts(request);
    Assertions.assertEquals(2, result.size());
    Assertions.assertEquals(responseList, result);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE_1);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE_2);
    Mockito.verify(autoApprovedRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE_3);
  }

  @Test
  public void testFetchSelectedItemsOfAutoApprovedProducts_EmptyProductCodes() {
    AutoApprovedSelectedDownloadRequest request = new AutoApprovedSelectedDownloadRequest();
    request.setProductCodes(new ArrayList<>());
    Assertions.assertThrows(ValidationException.class, () -> {
      autoApprovedService.fetchSelectedItemsOfAutoApprovedProducts(request);
    });
  }

}
