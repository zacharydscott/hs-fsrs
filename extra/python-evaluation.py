import sys
import json
from fsrs import Scheduler, Card, Rating, ReviewLog
from datetime import datetime, timedelta

def parse_time(ts: float) -> datetime:
    return datetime.utcfromtimestamp(ts)

def main():
    card = Card.from_dict(json.loads(sys.argv[1]))
    review_time = datetime.fromisoformat(sys.argv[2])
    rating = Rating(int(sys.argv[3]))

    scheduler = Scheduler()

    [reviewedCard, revLog] = scheduler.review_card(card,rating,review_datetime=review_time, review_duration=5)

    print(json.dumps({
      "out_card": Card.to_dict(reviewedCard),
      "out_revLog": ReviewLog.to_dict(revLog)
    }))

if __name__ == "__main__":
    main()
