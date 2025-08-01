import sys
import json
from fsrs import Scheduler, Card, Rating, ReviewLog
from datetime import datetime, timedelta

def parse_time(ts: float) -> datetime:
    return datetime.utcfromtimestamp(ts)

def main():
    scheduler = Scheduler(enable_fuzzing=False);
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            input = json.loads(line)
            card = Card.from_dict(input["in_card"])
            rating = Rating(input["in_rating"])
            review_time = datetime.fromisoformat(input["in_review_time"])
        except Exception as e:
            print(json.dumps({"error":f"Invalid input JSON '{line}': {e}"}))
            sys.stdout.flush()
            continue
        try:
            [reviewedCard, revLog] = scheduler.review_card(card,rating,review_datetime=review_time, review_duration=5)

            print(json.dumps({
              "out_card": Card.to_dict(reviewedCard),
              "out_revLog": ReviewLog.to_dict(revLog),
              "out_retrievability": scheduler.get_card_retrievability(card=card, current_datetime=review_time)
            }))
            sys.stdout.flush()
        except Exception as e:
            print(json.dumps({"error": str(e)}))
            sys.stdout.flush()

if __name__ == "__main__":
    main()
