package com.gdn.mta.product.service.converter;

import com.gdn.common.base.GdnPreconditions;

public abstract class BaseObjectConverter {

  protected void checkSourceObject(Object source) {
    GdnPreconditions.checkArgument(source != null, "Source object cannot be null" );
  }
}
