package com.gdn.partners.product.analytics.service.impl;


import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoApprovedProductsUserFeedback;
import com.gdn.partners.product.analytics.repository.UserFeedbackRepository;
import com.gdn.partners.product.analytics.service.impl.exception.ValidationException;
import com.gdn.partners.product.analytics.web.model.UserFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.request.OtherModelFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.UserFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.UserImageFeedbackRequest;

@ExtendWith(MockitoExtension.class)
class UserFeedbackServiceImplTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String USER_FEEDBACK = "user-feedback";
  private static final String BRAND_MISMATCH = "brand-mismatch";
  private static final String NOTES = "notes";


  @InjectMocks
  private UserFeedbackServiceImpl userFeedbackService;

  @Mock
  private UserFeedbackRepository userFeedbackRepository;

  @Mock
  private ObjectMapper objectMapper;

  private UserFeedbackRequest userFeedbackRequest;
  private AutoApprovedProductsUserFeedback autoApprovedProductsUserFeedback;

  @BeforeEach
  public void setup() {
    userFeedbackRequest = new UserFeedbackRequest();
    autoApprovedProductsUserFeedback = new AutoApprovedProductsUserFeedback();
    autoApprovedProductsUserFeedback.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(userFeedbackRepository);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductNewFeedbackTest() throws Exception {
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductExistingFeedbackTest() throws Exception {
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    OtherModelFeedbackRequest otherModelFeedbackRequest = new OtherModelFeedbackRequest();
    otherModelFeedbackRequest.setContentFeedBack(Collections.singleton(BRAND_MISMATCH));
    otherModelFeedbackRequest.setContentNotes(NOTES);
    userFeedbackRequest.setOtherModelFeedbackRequest(otherModelFeedbackRequest);
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(new ObjectMapper().writeValueAsString(userFeedbackRequest));
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class)))
        .thenReturn(userFeedbackRequest);
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class));
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }


  @Test
  public void updateUserFeedbackForAutoApprovedProductExistingFeedbackEmptyRequestTest() throws Exception {
    autoApprovedProductsUserFeedback.setUserFeedback(new ObjectMapper().writeValueAsString(userFeedbackRequest));
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class)))
        .thenReturn(userFeedbackRequest);
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class));
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductEmptyProductCode() throws Exception {
    Assertions.assertThrows(ValidationException.class,
        () -> userFeedbackService.updateUserFeedbackForAutoApprovedProduct(StringUtils.EMPTY, userFeedbackRequest));
  }

  @Test
  public void fetchUserFeedbackTest() throws Exception {
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    OtherModelFeedbackRequest otherModelFeedbackRequest = new OtherModelFeedbackRequest();
    otherModelFeedbackRequest.setContentFeedBack(Collections.singleton(BRAND_MISMATCH));
    otherModelFeedbackRequest.setContentNotes(NOTES);
    userFeedbackRequest.setOtherModelFeedbackRequest(otherModelFeedbackRequest);
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(new ObjectMapper().writeValueAsString(userFeedbackRequest));
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    UserFeedbackResponse response = userFeedbackService.fetchUserFeedbackResponse(PRODUCT_CODE);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(BRAND_MISMATCH,
        response.getOtherModelFeedback().getContentFeedBack().stream().findFirst()
            .orElse(StringUtils.EMPTY));
  }

  @Test
  public void fetchUserFeedbackNullOtherFeedbackResponseTest() throws Exception {
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(new ObjectMapper().writeValueAsString(userFeedbackRequest));
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    UserFeedbackResponse response = userFeedbackService.fetchUserFeedbackResponse(PRODUCT_CODE);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Assertions.assertNull(response.getOtherModelFeedback());
  }

  @Test
  public void fetchUserFeedbackNewFeedbackTest() throws Exception {
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    UserFeedbackResponse response = userFeedbackService.fetchUserFeedbackResponse(PRODUCT_CODE);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(0, response.getUserImageFeedback().size());
  }

  @Test
  public void fetchUserFeedbackExceptionTest() throws Exception {
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    OtherModelFeedbackRequest otherModelFeedbackRequest = new OtherModelFeedbackRequest();
    otherModelFeedbackRequest.setContentFeedBack(Collections.singleton(BRAND_MISMATCH));
    otherModelFeedbackRequest.setContentNotes(NOTES);
    userFeedbackRequest.setOtherModelFeedbackRequest(otherModelFeedbackRequest);
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(null);
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    userFeedbackService.fetchUserFeedbackResponse(PRODUCT_CODE);
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductWithOnlyImageFeedbackTest() throws Exception {
    // Setup
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(new ObjectMapper().writeValueAsString(new UserFeedbackRequest()));
    
    // Mock
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class)))
        .thenReturn(new UserFeedbackRequest());
    
    // Execute
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    
    // Verify
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class));
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductWithOnlyOtherModelFeedbackTest() throws Exception {
    // Setup
    OtherModelFeedbackRequest otherModelFeedbackRequest = new OtherModelFeedbackRequest();
    otherModelFeedbackRequest.setContentFeedBack(Collections.singleton(BRAND_MISMATCH));
    otherModelFeedbackRequest.setContentNotes(NOTES);
    userFeedbackRequest.setOtherModelFeedbackRequest(otherModelFeedbackRequest);
    autoApprovedProductsUserFeedback.setUserFeedback(new ObjectMapper().writeValueAsString(new UserFeedbackRequest()));
    
    // Mock
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class)))
        .thenReturn(new UserFeedbackRequest());
    
    // Execute
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    
    // Verify
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(UserFeedbackRequest.class));
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductWithEmptyExistingFeedbackTest() throws Exception {
    // Setup
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(StringUtils.EMPTY);
    
    // Mock
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    
    // Execute
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    
    // Verify
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }

  @Test
  public void updateUserFeedbackForAutoApprovedProductWithNullExistingFeedbackTest() throws Exception {
    // Setup
    UserImageFeedbackRequest userImageFeedbackRequest = new UserImageFeedbackRequest();
    userFeedbackRequest.setUserImageFeedback(List.of(userImageFeedbackRequest));
    autoApprovedProductsUserFeedback.setUserFeedback(null);
    
    // Mock
    Mockito.when(userFeedbackRepository.findByProductCode(PRODUCT_CODE)).thenReturn(autoApprovedProductsUserFeedback);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(USER_FEEDBACK);
    
    // Execute
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(PRODUCT_CODE, userFeedbackRequest);
    
    // Verify
    Mockito.verify(userFeedbackRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(userFeedbackRepository).save(Mockito.any());
  }

}
