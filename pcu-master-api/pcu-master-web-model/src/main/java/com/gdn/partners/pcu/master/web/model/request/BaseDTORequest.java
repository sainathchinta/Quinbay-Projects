package com.gdn.partners.pcu.master.web.model.request;

import java.util.Date;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author BhagwatiMalav - created on 02/11/18
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public abstract class BaseDTORequest extends BaseRequest{

  private static final long serialVersionUID = -446087358204764324L;

  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
}
