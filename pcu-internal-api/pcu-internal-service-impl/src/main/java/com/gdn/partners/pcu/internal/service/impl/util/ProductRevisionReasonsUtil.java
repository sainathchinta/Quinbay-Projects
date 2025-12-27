package com.gdn.partners.pcu.internal.service.impl.util;

import java.util.HashMap;
import java.util.Map;
import com.gdn.partners.pcu.internal.service.model.ProductRevisionType;

/**
 * Created by govind on 10/01/2019 AD.
 */
public class ProductRevisionReasonsUtil {

  private ProductRevisionReasonsUtil() {}

  public static final String REVISION_VARIANT_COMBINE_ID = "PRODUCT.REVISION-REASON.VARIANT.COMBINE";
  public static final String REVISION_WRONG_IMAGE_ID = "PRODUCT.REVISION-REASON.WRONG-IMAGE";
  public static final String REVISION_WRONG_CONTENT_ID = "PRODUCT.REVISION-REASON.WRONG-CONTENT";
  public static final String REVISION_WRONG_BRAND_ID = "PRODUCT.REVISION-REASON.WRONG-BRAND";

  public static Map<String, String> getProductRevisionReasons(String type) {
    Map<String, String> reasons = new HashMap<>();
    if (ProductRevisionType.REVISION_TYPE_SCREENING.getName().equals(type)) {
      reasons.put(REVISION_VARIANT_COMBINE_ID, "Varian seharusnya digabung");
      reasons.put(REVISION_WRONG_IMAGE_ID, "Foto tidak sesuai atau berkualitas rendah");
      reasons.put(REVISION_WRONG_CONTENT_ID, "Konten tidak lengkap atau tidak sesuai");
      reasons.put(REVISION_WRONG_BRAND_ID, "Salah brand");
    }
    return reasons;
  }
}
