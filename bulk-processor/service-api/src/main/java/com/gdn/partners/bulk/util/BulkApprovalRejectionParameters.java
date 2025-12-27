package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;

public interface BulkApprovalRejectionParameters {
  String PRODUCT_CODE = "Product Code";
  String APPROVAL_REASON = "Approval Reason";
  String REJECTION_REASON = "Rejection Reason";
  String COMMENTS_TO_SELLERS = "Comments to Sellers";
  Collection<String> BULK_APPROVAL = Arrays.asList(PRODUCT_CODE, APPROVAL_REASON);
  Collection<String> BULK_REJECTION =
    Arrays.asList(PRODUCT_CODE, REJECTION_REASON, COMMENTS_TO_SELLERS);
}
