package com.gdn.aggregate.platform.module.product.listener.service.helper;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.PartialClasses;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialProductL3;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialProductL4;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialProductReviewChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialEndSivaCampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialCleanSivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialCleanSivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialStoreClosingMerchantChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.partial.PartialTags;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component("ProductPartialUpdateService")
public class PartialUpdateService {

  private boolean isAllowedToPartialUpdate(SaveParam saveParam) {
    return Optional.ofNullable(saveParam)
        .map(ParamUtil::getSourceClassName)
        .filter(PartialClasses.ALLOWED_CLASSES::contains)
        .isPresent();
  }

  public <T extends BaseData> T toPartialData(T processedData, SaveParam saveParam) {
    if (!isAllowedToPartialUpdate(saveParam)) {
      return null;
    }
    switch (ParamUtil.getSourceClassName(saveParam)) {
      case PartialClasses.PRODUCT_REVIEW_CHANGE_EVENT :
        return toPartialProductReviewChangeEvent(processedData);
      case PartialClasses.STORE_CLOSING_CHANGE_EVENT :
        return toPartialStoreClosingMerchantChangeEvent(processedData);
      case PartialClasses.PRODUCT :
        return toPartialProduct(processedData,saveParam);
      case PartialClasses.TAGS :
        return toPartialTags(processedData);
      case PartialClasses.CLEAN_SIVA_FLASHSALE_SCHEDULE:
      case PartialClasses.DEACTIVATE_SIVA_FLASHSALE_SCHEDULE:
        return toPartialCleanSivaFlashsaleSchedule(processedData);
      case PartialClasses.CLEAN_SIVA_FLASHSALE_GROUP:
      case PartialClasses.DEACTIVATE_SIVA_FLASHSALE_GROUP:
        return toPartialCleanSivaFlashsaleGroup(processedData);
      case PartialClasses.END_SIVA_CAMPAIGN_PRODUCT:
        return toPartialSivaCampaignProduct(processedData);
    }
    return processedData;
  }

  private <T extends BaseData> T toPartialProductReviewChangeEvent(T processedData) {
    PartialProductReviewChangeEvent partialData = new PartialProductReviewChangeEvent();
    BeanUtils.copyProperties(processedData,partialData);
    return (T) partialData;
  }

  private <T extends BaseData> T toPartialStoreClosingMerchantChangeEvent(T processedData) {
    PartialStoreClosingMerchantChangeEvent partialData = new PartialStoreClosingMerchantChangeEvent();
    BeanUtils.copyProperties(processedData,partialData);
    return (T) partialData;
  }

  private <T extends BaseData> T toPartialProduct(T processedData, SaveParam saveParam) {
    if (ModuleProductUtil.isDirectUpdateSivaProduct(saveParam)) {
      PartialProductL3 partialData = new PartialProductL3();
      BeanUtils.copyProperties(processedData,partialData);
      return (T) partialData;
    } else {
      PartialProductL4 partialData = new PartialProductL4();
      BeanUtils.copyProperties(processedData,partialData);
      return (T) partialData;
    }
  }

  private <T extends BaseData> T toPartialTags(T processedData) {
    PartialTags partialData = new PartialTags();
    BeanUtils.copyProperties(processedData,partialData);
    return (T) partialData;
  }

  private <T extends BaseData> T toPartialCleanSivaFlashsaleSchedule(T processedData) {
    PartialCleanSivaFlashsaleSchedule partialData = new PartialCleanSivaFlashsaleSchedule();
    BeanUtils.copyProperties(processedData,partialData);
    return (T) partialData;
  }

  private <T extends BaseData> T toPartialCleanSivaFlashsaleGroup(T processedData) {
    PartialCleanSivaFlashsaleGroup partialData = new PartialCleanSivaFlashsaleGroup();
    BeanUtils.copyProperties(processedData,partialData);
    return (T) partialData;
  }

  private <T extends BaseData> T toPartialSivaCampaignProduct(T processedData) {
    PartialEndSivaCampaignProduct partialData = new PartialEndSivaCampaignProduct();
    BeanUtils.copyProperties(processedData,partialData);
    return (T) partialData;
  }

}
