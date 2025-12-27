package com.gdn.partners.pbp.outbound.merchantEducation;

public interface MerchantEducationOutbound {

  NotificationSettings findByUsernameAndStoreCode(String storeId, String userName, String storeCode) throws Exception;
}
