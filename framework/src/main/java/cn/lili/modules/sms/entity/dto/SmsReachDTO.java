package cn.lili.modules.sms.entity.dto;

import cn.lili.modules.sms.entity.dos.SmsReach;
import lombok.Data;

import java.util.List;

/**
 * 短信任务DTO
 * @author Chopper
 * @since 2020/12/8 9:46
 */
@Data
public class SmsReachDTO extends SmsReach {

    private List<String> mobile;
}
