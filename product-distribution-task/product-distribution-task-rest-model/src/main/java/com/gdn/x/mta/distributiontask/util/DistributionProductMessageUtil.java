package com.gdn.x.mta.distributiontask.util;

/**
 * Created by virajjasani on 21/09/16.
 */
public class DistributionProductMessageUtil {

  public static final String PRODUCT_CODE_EMPTY = "Product Code should not be empty";
  public static final String NOTES_EMPTY = "Reason should not be empty";
  public static final String PRODUCT_NAME_EMPTY = "Product Name should not be empty";
  public static final String CREATED_DATE_EMPTY = "Created Date should not be empty";
  public static final String PRODUCT_STATE_INVALID = "Product State is invalid";
  public static final String PRODUCT_NOT_FOUND_ERROR = "Product with given productCode not found";
  public static final String PRODUCTID_NOT_FOUNT_ERROR = "Product with gievn productId not found";
  public static final String PRODUCT_UNASSIGNED_TO_VENDOR_ERROR =
      "Product is not assigned to given Vendor";
  public static final String PRODUCT_UNASSIGNED_STATUS_ERROR =
      "Product is not assigned to any vendor";
  public static final String VENDOR_CODE_EMPTY_ERROR = "Vendor code is empty";
  public static final String REJECT_PRODUCT_DETAIL_ERROR = "Reject product details are empty";
  public static final String  REJECT_REASON_NULL_ERROR = "Reject reason is null";
  public static final String REJECT_REASON_OF_PRODUCT_NULL_OR_EMPTY =
      "Reject reason cannot be null or empty";
  public static final String VENDOR_REJECT_UNAUTHORIZED =
      "Vendor is not authorized to reject product";
  public static final String APPROVE_VALUE_INCORRECT =
      "approve value should be either image or content";
  public static final String PDT_TASK_NOT_PRESENT =
      "ProductDistributionTask does not exist for given Product";
  public static final String DESCRIPTION_MUST_NOT_BE_BLANK =
      "Product description should not be empty";
  public static final String LONG_DESCRIPTION_MUST_NOT_BE_BLANK =
      "Product long description should not be empty";
  public static final String UNIQUE_SELLING_POINT_MUST_NOT_BE_BLANK =
      "Product unique selling point should not be empty";
  public static final String LENGTH_MUST_NOT_BE_BLANK = "Product length should not be empty";
  public static final String WIDTH_MUST_NOT_BE_BLANK = "Product width should not be empty";
  public static final String HEIGHT_MUST_NOT_BE_BLANK = "Product height should not be empty";
  public static final String WEIGHT_MUST_NOT_BE_BLANK = "Product width should not be empty";
  public static final String SHIPPING_WEIGHT_MUST_NOT_BE_BLANK =
      "Product shipping weight should not be empty";
  public static final String PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK =
      "Product attributes should not be empty";
  public static final String PRODUCT_IMAGES_EMPTY = "Product images should not be empty";
  public static final String PRODUCT_ALREADY_APPROVED = "Product has already been approved";
  public static final String NOT_VALID_MONTH_VALUE = "Not a valid month value";
  public static final String ATTRIBUTE_VALUE_LENGTH_EXCEEDED_LENGTH =
      "product attribute value length should not exceed 255 character.";
  public static final String ACTION_VALUE_INCORRECT = "action value should be either assign or unassign";
  public static final String TYPE_VALUE_INCORRECT = "type value should be either image or content";
  public static final String IMAGE_NOT_FOUND = "Uploaded image not found";
  public static final String ACTIVE_IMAGE_NOT_FOUND = "Active image not found";

}
