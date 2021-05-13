package cn.lili.modules.message.entity.dto;

import cn.lili.modules.message.entity.dos.SmsReach;
import lombok.Data;

import java.util.List;

/**
 * 短信任务DTO
 * @author Chopper
 * @date 2020/12/8 9:46
 */
@Data
public class SmsReachDTO extends SmsReach {

    private List<String> mobile;
}
