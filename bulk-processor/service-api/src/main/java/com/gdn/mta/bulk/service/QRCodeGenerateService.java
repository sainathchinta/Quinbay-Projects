package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkUpdateEventModel;

public interface QRCodeGenerateService {

  void generateQRCode(BulkUpdateEventModel eventModel) throws Exception;
}
