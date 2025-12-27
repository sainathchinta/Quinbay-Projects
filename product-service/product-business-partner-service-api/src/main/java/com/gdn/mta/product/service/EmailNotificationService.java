package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCollectionElement;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.entity.ProductWfState;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface EmailNotificationService {

   void sendEmailDeleteProductBusinessPartner(ProductDetailResponse product,
       Page<ProductBusinessPartnerResponse> productBusinessPartners, String notes, String productName) throws Exception;

   void sendEmailForProductLive(String storeId, List<ProfileResponse> profileList,
       Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap);

   void sendFailedRetryProductsMail(List<ProductLevel3FailedEntity> failedRetryProductList);

   void sendExceedActivationEmail(ProfileResponse profileResponse, String username,
       ProductBusinessPartnerConfigRequest request, List<ProductLevel3WipDTO> productLevel3Wips,
       String submissionDateString);

   void sendEmailToBusinessPartnerForDeletedProducts(String storeId, String businessPartnerCode,
       List<ProductCollection> productCollections);

   void sendProductStuckAlertMail(List<ProductWfState> productsAboveRetryCount, Integer batchSize);
}
