package com.gdn.x.productcategorybase.service.brand;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.BrandAuthorisationWipAction;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.NearExpiryModelEvent;
import com.gdn.x.productcategorybase.dto.BrandAuthorisationStatusIdDTO;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Component
@Slf4j
@RequiredArgsConstructor
public class BrandAuthorisationWipServiceWrapperImpl implements BrandAuthorisationWipServiceWrapper {

  @Value("${brand.auth.send.email}")
  private boolean brandAuthSendEmail;
  public static final String BRAND_AUTH_ACTION = "brand-auth-action";
  private final BrandAuthorisationWipService brandAuthorisationWipService;
  private final SystemParameterService systemParameterService;
  private final ApplicationCacheServiceBean applicationCacheServiceBean;
  private final DomainEventPublisherService domainEventPublisherService;
  private final MailDeliveryService mailDeliveryService;


  @Override
  public void brandAuthorisationWipAction(String storeId, String username,
    BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest) {

    Pair<List<BrandAuthorisationHistory>, BrandAuthorisationStatusIdDTO> pair =
        brandAuthorisationWipService.brandAuthorisationWipAction(storeId, username,
            brandAuthorisationWipActionRequest);
    List<BrandAuthorisationHistory> brandAuthorisationHistoryList = pair.getLeft();
    BrandAuthorisationStatusIdDTO statusIdPairDto = pair.getRight();
    try {
      //Clear cache only in case of Approval
      if (BrandAuthorisationWipAction.valueOf(brandAuthorisationWipActionRequest.getAction())
        .equals(BrandAuthorisationWipAction.APPROVE) && CollectionUtils.isNotEmpty(
        brandAuthorisationHistoryList)) {
        applicationCacheServiceBean.evictBrandAuthorizationCache(
          brandAuthorisationWipActionRequest.getBrandCode());
      }
      brandAuthorisationHistoryList.forEach(
        historyEventModel -> domainEventPublisherService.publishBrandAuthHistoryEvent(
          historyEventModel.getBrandCode(), historyEventModel.getSellerCode(), historyEventModel));
      if (brandAuthSendEmail) {
        mailDeliveryService.sendBrandAuthorisationActionMail(
            brandAuthorisationWipActionRequest.getSellerCode(), BRAND_AUTH_ACTION,
            brandAuthorisationWipActionRequest.getAction(), statusIdPairDto.getStatus(),
            statusIdPairDto.getId(), new ArrayList<>());
      }
    } catch (Exception e) {
      log.error("Error while evicting cache and publishing history update for brand code {} and "
          + "seller code {} ", brandAuthorisationWipActionRequest.getBrandCode(),
          brandAuthorisationWipActionRequest.getSellerCode());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        ErrorMessage.EVICT_CACHE_AND_HISTORY_UPDATE_FAILED.getMessage());
    }
  }

  @Override
  public boolean checkEligibility(String storeId, String sellerCode){
    SystemParameter systemParameter = systemParameterService.findByStoreIdAndVariable(storeId,
        Constants.MAX_PENDING_BRAND_AUTH_WIP_REQUESTS);
    long maxThresholdForPendingBrandAuthWipRequests = Long.parseLong(systemParameter.getValue());
    long countOfPendingBrandAuthWipRequests =
        brandAuthorisationWipService.fetchCountOfPendingRequestsForSeller(storeId, sellerCode);
    return countOfPendingBrandAuthWipRequests < maxThresholdForPendingBrandAuthWipRequests;
  }

  @Override
  public void submitBrandAuthorisationRequest(String storeId, String username,
    BrandAuthUpdateRequest brandAuthUpdateRequest) throws Exception {
    DateFormat historyDateFormat = new SimpleDateFormat(Constants.STANDARD_DATE_PATTERN);
    brandAuthorisationWipService.submitBrandAuthorisationRequest(storeId, brandAuthUpdateRequest);
    BrandAuthorisationHistory brandAuthHistory =
      ConverterUtil.convertToBrandAuthRequestedHistory(brandAuthUpdateRequest, historyDateFormat);
    brandAuthHistory.setUpdatedBy(username);
    brandAuthHistory.setStoreId(storeId);
    domainEventPublisherService.publishBrandAuthHistoryEvent(brandAuthHistory.getBrandCode(),
      brandAuthHistory.getSellerCode(), brandAuthHistory);

  }

  @Override
  public void sendNearExpiryMailNotification(String storeId) {
    List<BrandAuthorisation> brandAuthorisations =
        brandAuthorisationWipService.fetchBrandAuthorisationForNearExpiry(storeId);

    Map<String, List<NearExpiryModelEvent>> eventsGroupedBySellerCode = new HashMap<>();
    for (BrandAuthorisation auth : brandAuthorisations) {
      NearExpiryModelEvent event = new NearExpiryModelEvent();
      event.setBrandName(auth.getBrandName());
      event.setAuthExpiryDate(auth.getAuthExpireDate());

      String sellerCode = auth.getSellerCode();
      eventsGroupedBySellerCode.computeIfAbsent(sellerCode, eventList -> new ArrayList<>()).add(event);
    }

    for (Map.Entry<String, List<NearExpiryModelEvent>> entry :
        eventsGroupedBySellerCode.entrySet()) {
      String sellerCode = entry.getKey();
      List<NearExpiryModelEvent> nearExpiryModelEvents = entry.getValue();

      mailDeliveryService.sendBrandAuthorisationActionMail(sellerCode, BRAND_AUTH_ACTION,
          BrandAuthorisationStatus.NEAR_EXPIRY.name(), BrandAuthorisationStatus.NEAR_EXPIRY.name(),
          null, nearExpiryModelEvents);
    }
  }
}
